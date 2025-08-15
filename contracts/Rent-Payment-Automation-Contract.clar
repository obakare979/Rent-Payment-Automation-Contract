(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_LEASE_NOT_FOUND (err u102))
(define-constant ERR_LEASE_EXPIRED (err u103))
(define-constant ERR_PAYMENT_ALREADY_MADE (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_LEASE_ALREADY_EXISTS (err u107))
(define-constant ERR_PROPERTY_ALREADY_EXISTS (err u108))

(define-constant ERR_SCHEDULE_NOT_FOUND (err u109))
(define-constant ERR_SCHEDULE_ALREADY_EXISTS (err u110))
(define-constant ERR_SCHEDULE_DISABLED (err u111))

(define-constant ERR_REQUEST_NOT_FOUND (err u112))
(define-constant ERR_REQUEST_ALREADY_EXISTS (err u113))
(define-constant ERR_INSUFFICIENT_MAINTENANCE_FUNDS (err u114))
(define-constant ERR_REQUEST_ALREADY_PROCESSED (err u115))


(define-map properties
  { property-id: uint }
  {
    landlord: principal,
    rent-amount: uint,
    penalty-rate: uint,
    grace-period: uint,
    active: bool
  }
)

(define-map leases
  { property-id: uint, tenant: principal }
  {
    start-block: uint,
    end-block: uint,
    monthly-rent: uint,
    last-payment-block: uint,
    security-deposit: uint,
    active: bool
  }
)

(define-map rent-payments
  { property-id: uint, tenant: principal, payment-month: uint }
  {
    amount-paid: uint,
    payment-block: uint,
    penalty-applied: uint,
    late: bool
  }
)

(define-map tenant-balances
  { tenant: principal }
  { balance: uint }
)

(define-data-var property-counter uint u0)

(define-public (register-property (rent-amount uint) (penalty-rate uint) (grace-period uint))
  (let ((property-id (+ (var-get property-counter) u1)))
    (asserts! (> rent-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? properties { property-id: property-id })) ERR_PROPERTY_ALREADY_EXISTS)
    (map-set properties
      { property-id: property-id }
      {
        landlord: tx-sender,
        rent-amount: rent-amount,
        penalty-rate: penalty-rate,
        grace-period: grace-period,
        active: true
      }
    )
    (var-set property-counter property-id)
    (ok property-id)
  )
)

(define-public (create-lease (property-id uint) (tenant principal) (lease-duration uint) (security-deposit uint))
  (let ((property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND)))
    (asserts! (is-eq (get landlord property) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (get active property) ERR_PROPERTY_NOT_FOUND)
    (asserts! (is-none (map-get? leases { property-id: property-id, tenant: tenant })) ERR_LEASE_ALREADY_EXISTS)
    (map-set leases
      { property-id: property-id, tenant: tenant }
      {
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height lease-duration),
        monthly-rent: (get rent-amount property),
        last-payment-block: u0,
        security-deposit: security-deposit,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (deposit-funds (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? tenant-balances { tenant: tx-sender })))))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set tenant-balances
      { tenant: tx-sender }
      { balance: (+ current-balance amount) }
    )
    (ok true)
  )
)

(define-public (pay-rent (property-id uint))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tx-sender }) ERR_LEASE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (tenant-balance (default-to u0 (get balance (map-get? tenant-balances { tenant: tx-sender }))))
    (current-month (get-current-month (get start-block lease)))
    (rent-amount (get monthly-rent lease))
    (is-late (is-payment-late property-id (get start-block lease) (get grace-period property)))
    (penalty-amount (if is-late (calculate-penalty rent-amount (get penalty-rate property)) u0))
    (total-amount (+ rent-amount penalty-amount))
  )
    (asserts! (get active lease) ERR_LEASE_EXPIRED)
    (asserts! (< stacks-block-height (get end-block lease)) ERR_LEASE_EXPIRED)
    (asserts! (>= tenant-balance total-amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (is-none (map-get? rent-payments { property-id: property-id, tenant: tx-sender, payment-month: current-month })) ERR_PAYMENT_ALREADY_MADE)
    
    (map-set tenant-balances
      { tenant: tx-sender }
      { balance: (- tenant-balance total-amount) }
    )
    
    (map-set rent-payments
      { property-id: property-id, tenant: tx-sender, payment-month: current-month }
      {
        amount-paid: total-amount,
        payment-block: stacks-block-height,
        penalty-applied: penalty-amount,
        late: is-late
      }
    )
    
    (map-set leases
      { property-id: property-id, tenant: tx-sender }
      (merge lease { last-payment-block: stacks-block-height })
    )
    
    (try! (as-contract (stx-transfer? total-amount tx-sender (get landlord property))))
    (ok true)
  )
)

(define-public (withdraw-security-deposit (property-id uint) (tenant principal))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tenant }) ERR_LEASE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (deposit-amount (get security-deposit lease))
  )
    (asserts! (is-eq (get landlord property) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> stacks-block-height (get end-block lease)) ERR_LEASE_EXPIRED)
    
    (map-set leases
      { property-id: property-id, tenant: tenant }
      (merge lease { security-deposit: u0, active: false })
    )
    
    (as-contract (stx-transfer? deposit-amount tx-sender tenant))
  )
)

(define-public (terminate-lease (property-id uint) (tenant principal))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tenant }) ERR_LEASE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
  )
    (asserts! (is-eq (get landlord property) tx-sender) ERR_UNAUTHORIZED)
    (map-set leases
      { property-id: property-id, tenant: tenant }
      (merge lease { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-lease (property-id uint) (tenant principal))
  (map-get? leases { property-id: property-id, tenant: tenant })
)

(define-read-only (get-payment-record (property-id uint) (tenant principal) (payment-month uint))
  (map-get? rent-payments { property-id: property-id, tenant: tenant, payment-month: payment-month })
)

(define-read-only (get-tenant-balance (tenant principal))
  (default-to u0 (get balance (map-get? tenant-balances { tenant: tenant })))
)

(define-read-only (calculate-penalty (rent-amount uint) (penalty-rate uint))
  (/ (* rent-amount penalty-rate) u100)
)

(define-read-only (get-current-month (start-block uint))
  (/ (- stacks-block-height start-block) u4320)
)

(define-read-only (is-payment-late (property-id uint) (start-block uint) (grace-period uint))
  (let (
    (current-month (get-current-month start-block))
    (month-start-block (+ start-block (* current-month u4320)))
    (grace-end-block (+ month-start-block grace-period))
  )
    (> stacks-block-height grace-end-block)
  )
)

(define-read-only (get-total-properties)
  (var-get property-counter)
)

(define-read-only (calculate-monthly-due (property-id uint) (tenant principal))
  (match (map-get? leases { property-id: property-id, tenant: tenant })
    lease (match (map-get? properties { property-id: property-id })
      property (let (
        (rent-amount (get monthly-rent lease))
        (is-late (is-payment-late property-id (get start-block lease) (get grace-period property)))
        (penalty-amount (if is-late (calculate-penalty rent-amount (get penalty-rate property)) u0))
      )
        (ok (+ rent-amount penalty-amount))
      )
      ERR_PROPERTY_NOT_FOUND
    )
    ERR_LEASE_NOT_FOUND
  )
)

(define-map auto-payment-schedules
  { property-id: uint, tenant: principal }
  {
    enabled: bool,
    payment-day: uint,
    last-execution: uint,
    created-block: uint,
    emergency-disabled: bool
  }
)

(define-public (schedule-auto-payment (property-id uint) (payment-day uint))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tx-sender }) ERR_LEASE_NOT_FOUND))
  )
    (asserts! (get active lease) ERR_LEASE_EXPIRED)
    (asserts! (and (>= payment-day u1) (<= payment-day u28)) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? auto-payment-schedules { property-id: property-id, tenant: tx-sender })) ERR_SCHEDULE_ALREADY_EXISTS)
    
    (map-set auto-payment-schedules
      { property-id: property-id, tenant: tx-sender }
      {
        enabled: true,
        payment-day: payment-day,
        last-execution: u0,
        created-block: stacks-block-height,
        emergency-disabled: false
      }
    )
    (ok true)
  )
)

(define-public (execute-scheduled-payment (property-id uint) (tenant principal))
  (let (
    (schedule (unwrap! (map-get? auto-payment-schedules { property-id: property-id, tenant: tenant }) ERR_SCHEDULE_NOT_FOUND))
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tenant }) ERR_LEASE_NOT_FOUND))
  )
    (asserts! (get enabled schedule) ERR_SCHEDULE_DISABLED)
    (asserts! (not (get emergency-disabled schedule)) ERR_SCHEDULE_DISABLED)
    (asserts! (get active lease) ERR_LEASE_EXPIRED)
    (asserts! (is-payment-due property-id tenant (get payment-day schedule)) ERR_PAYMENT_ALREADY_MADE)
    
    (map-set auto-payment-schedules
      { property-id: property-id, tenant: tenant }
      (merge schedule { last-execution: stacks-block-height })
    )
    (try! (pay-rent property-id))
    (ok true)
  )
)

(define-public (emergency-disable-schedule (property-id uint) (tenant principal))
  (let (
    (schedule (unwrap! (map-get? auto-payment-schedules { property-id: property-id, tenant: tenant }) ERR_SCHEDULE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
  )
    (asserts! (is-eq (get landlord property) tx-sender) ERR_UNAUTHORIZED)
    (map-set auto-payment-schedules
      { property-id: property-id, tenant: tenant }
      (merge schedule { emergency-disabled: true })
    )
    (ok true)
  )
)

(define-read-only (get-payment-schedule (property-id uint) (tenant principal))
  (map-get? auto-payment-schedules { property-id: property-id, tenant: tenant })
)

(define-read-only (is-payment-due (property-id uint) (tenant principal) (payment-day uint))
  (match (map-get? leases { property-id: property-id, tenant: tenant })
    lease (let (
      (current-month (get-current-month (get start-block lease)))
      (payment-record (map-get? rent-payments { property-id: property-id, tenant: tenant, payment-month: current-month }))
    )
      (and 
        (>= (mod (- stacks-block-height (get start-block lease)) u4320) payment-day)
        (is-none payment-record)
      )
    )
    false
  )
)

(define-map maintenance-funds
  { property-id: uint, tenant: principal }
  { balance: uint }
)

(define-map maintenance-requests
  { property-id: uint, request-id: uint }
  {
    landlord: principal,
    tenant: principal,
    description: (string-ascii 200),
    estimated-cost: uint,
    status: (string-ascii 20),
    submitted-block: uint,
    processed-block: uint
  }
)

(define-data-var request-counter uint u0)

(define-public (deposit-maintenance-funds (property-id uint) (amount uint))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tx-sender }) ERR_LEASE_NOT_FOUND))
    (current-balance (default-to u0 (get balance (map-get? maintenance-funds { property-id: property-id, tenant: tx-sender }))))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get active lease) ERR_LEASE_EXPIRED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set maintenance-funds
      { property-id: property-id, tenant: tx-sender }
      { balance: (+ current-balance amount) }
    )
    (ok true)
  )
)

(define-public (submit-maintenance-request (property-id uint) (tenant principal) (description (string-ascii 200)) (estimated-cost uint))
  (let (
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (request-id (+ (var-get request-counter) u1))
    (maintenance-balance (default-to u0 (get balance (map-get? maintenance-funds { property-id: property-id, tenant: tenant }))))
  )
    (asserts! (is-eq (get landlord property) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> estimated-cost u0) ERR_INVALID_AMOUNT)
    (asserts! (>= maintenance-balance estimated-cost) ERR_INSUFFICIENT_MAINTENANCE_FUNDS)
    (map-set maintenance-requests
      { property-id: property-id, request-id: request-id }
      {
        landlord: tx-sender,
        tenant: tenant,
        description: description,
        estimated-cost: estimated-cost,
        status: "pending",
        submitted-block: stacks-block-height,
        processed-block: u0
      }
    )
    (var-set request-counter request-id)
    (ok request-id)
  )
)

(define-public (process-maintenance-request (property-id uint) (request-id uint) (approve bool))
  (let (
    (request (unwrap! (map-get? maintenance-requests { property-id: property-id, request-id: request-id }) ERR_REQUEST_NOT_FOUND))
    (maintenance-balance (default-to u0 (get balance (map-get? maintenance-funds { property-id: property-id, tenant: tx-sender }))))
  )
    (asserts! (is-eq (get tenant request) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status request) "pending") ERR_REQUEST_ALREADY_PROCESSED)
    (if approve
      (begin
        (asserts! (>= maintenance-balance (get estimated-cost request)) ERR_INSUFFICIENT_MAINTENANCE_FUNDS)
        (map-set maintenance-funds
          { property-id: property-id, tenant: tx-sender }
          { balance: (- maintenance-balance (get estimated-cost request)) }
        )
        (try! (as-contract (stx-transfer? (get estimated-cost request) tx-sender (get landlord request))))
        (map-set maintenance-requests
          { property-id: property-id, request-id: request-id }
          (merge request { status: "approved", processed-block: stacks-block-height })
        )
      )
      (map-set maintenance-requests
        { property-id: property-id, request-id: request-id }
        (merge request { status: "rejected", processed-block: stacks-block-height })
      )
    )
    (ok true)
  )
)

(define-read-only (get-maintenance-balance (property-id uint) (tenant principal))
  (default-to u0 (get balance (map-get? maintenance-funds { property-id: property-id, tenant: tenant })))
)

(define-read-only (get-maintenance-request (property-id uint) (request-id uint))
  (map-get? maintenance-requests { property-id: property-id, request-id: request-id })
)