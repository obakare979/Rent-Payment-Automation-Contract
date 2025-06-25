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
    
    (as-contract (stx-transfer? total-amount tx-sender (get landlord property)))
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
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tenant }) ERR_LEASE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (rent-amount (get monthly-rent lease))
    (is-late (is-payment-late property-id (get start-block lease) (get grace-period property)))
    (penalty-amount (if is-late (calculate-penalty rent-amount (get penalty-rate property)) u0))
  )
    (ok (+ rent-amount penalty-amount))
  )
)
