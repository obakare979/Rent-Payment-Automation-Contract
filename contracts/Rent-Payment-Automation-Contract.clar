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

(define-constant ERR_DISPUTE_NOT_FOUND (err u116))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u117))
(define-constant ERR_INVALID_ARBITER (err u118))
(define-constant ERR_DISPUTE_NOT_ACTIVE (err u119))

(define-constant ERR_RENEWAL_NOT_ELIGIBLE (err u120))
(define-constant ERR_INVALID_TIER_CONFIG (err u121))
(define-constant ERR_RENEWAL_TOO_EARLY (err u122))

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


(define-map property-analytics
  { property-id: uint }
  {
    total-payments: uint,
    on-time-payments: uint,
    total-maintenance-requests: uint,
    avg-response-time: uint,
    landlord-rating: uint,
    landlord-rating-count: uint,
    created-block: uint
  }
)

(define-map tenant-performance
  { property-id: uint, tenant: principal }
  {
    payment-streak: uint,
    late-payments: uint,
    maintenance-cooperation: uint,
    tenant-rating: uint,
    tenant-rating-count: uint,
    performance-score: uint
  }
)

(define-map property-ratings
  { property-id: uint, rater: principal, rating-id: uint }
  {
    rating-score: uint,
    rating-type: (string-ascii 10),
    comment-hash: (buff 32),
    submitted-block: uint,
    verified: bool
  }
)

(define-data-var rating-counter uint u0)

(define-public (submit-property-rating (property-id uint) (score uint) (rating-type (string-ascii 10)) (comment-hash (buff 32)))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tx-sender }) ERR_LEASE_NOT_FOUND))
    (rating-id (+ (var-get rating-counter) u1))
    (current-analytics (default-to 
      { total-payments: u0, on-time-payments: u0, total-maintenance-requests: u0, avg-response-time: u0, landlord-rating: u0, landlord-rating-count: u0, created-block: stacks-block-height }
      (map-get? property-analytics { property-id: property-id })
    ))
  )
    (asserts! (and (>= score u1) (<= score u5)) ERR_INVALID_AMOUNT)
    (asserts! (not (get active lease)) ERR_LEASE_EXPIRED)
    (map-set property-ratings
      { property-id: property-id, rater: tx-sender, rating-id: rating-id }
      {
        rating-score: score,
        rating-type: rating-type,
        comment-hash: comment-hash,
        submitted-block: stacks-block-height,
        verified: true
      }
    )
    (map-set property-analytics
      { property-id: property-id }
      (merge current-analytics {
        landlord-rating: (/ (+ (* (get landlord-rating current-analytics) (get landlord-rating-count current-analytics)) score) (+ (get landlord-rating-count current-analytics) u1)),
        landlord-rating-count: (+ (get landlord-rating-count current-analytics) u1)
      })
    )
    (var-set rating-counter rating-id)
    (ok rating-id)
  )
)

(define-public (update-payment-analytics (property-id uint) (tenant principal) (on-time bool))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tenant }) ERR_LEASE_NOT_FOUND))
    (current-analytics (default-to 
      { total-payments: u0, on-time-payments: u0, total-maintenance-requests: u0, avg-response-time: u0, landlord-rating: u0, landlord-rating-count: u0, created-block: stacks-block-height }
      (map-get? property-analytics { property-id: property-id })
    ))
    (current-performance (default-to 
      { payment-streak: u0, late-payments: u0, maintenance-cooperation: u5, tenant-rating: u0, tenant-rating-count: u0, performance-score: u100 }
      (map-get? tenant-performance { property-id: property-id, tenant: tenant })
    ))
  )
    (map-set property-analytics
      { property-id: property-id }
      (merge current-analytics {
        total-payments: (+ (get total-payments current-analytics) u1),
        on-time-payments: (if on-time (+ (get on-time-payments current-analytics) u1) (get on-time-payments current-analytics))
      })
    )
    (map-set tenant-performance
      { property-id: property-id, tenant: tenant }
      (merge current-performance {
        payment-streak: (if on-time (+ (get payment-streak current-performance) u1) u0),
        late-payments: (if on-time (get late-payments current-performance) (+ (get late-payments current-performance) u1)),
        performance-score: (calculate-performance-score property-id tenant on-time)
      })
    )
    (ok true)
  )
)

(define-read-only (get-property-analytics (property-id uint))
  (map-get? property-analytics { property-id: property-id })
)

(define-read-only (get-tenant-performance (property-id uint) (tenant principal))
  (map-get? tenant-performance { property-id: property-id, tenant: tenant })
)

(define-read-only (calculate-performance-score (property-id uint) (tenant principal) (latest-on-time bool))
  (match (map-get? tenant-performance { property-id: property-id, tenant: tenant })
    performance (let (
      (streak-bonus (if   latest-on-time (* (get payment-streak performance) u5) u50))
      (late-penalty (if latest-on-time u60 (* (get late-payments performance) u10)))
      (base-score u100)
    )
      (+ (- base-score late-penalty) streak-bonus)
    )
    u100
  )
)

(define-read-only (get-property-rating (property-id uint) (rater principal) (rating-id uint))
  (map-get? property-ratings { property-id: property-id, rater: rater, rating-id: rating-id })
)

(define-map rent-escrow-disputes
  { property-id: uint, tenant: principal, dispute-id: uint }
  {
    initiator: principal,
    dispute-type: (string-ascii 30),
    escrow-amount: uint,
    arbiter: (optional principal),
    status: (string-ascii 20),
    evidence-hash: (buff 32),
    submitted-block: uint,
    resolved-block: uint,
    resolution: (string-ascii 50)
  }
)

(define-data-var dispute-counter uint u0)

(define-public (initiate-rent-dispute (property-id uint) (tenant principal) (dispute-type (string-ascii 30)) (evidence-hash (buff 32)))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tenant }) ERR_LEASE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (dispute-id (+ (var-get dispute-counter) u1))
    (rent-amount (get monthly-rent lease))
    (caller-is-landlord (is-eq tx-sender (get landlord property)))
    (caller-is-tenant (is-eq tx-sender tenant))
  )
    (asserts! (or caller-is-landlord caller-is-tenant) ERR_UNAUTHORIZED)
    (asserts! (get active lease) ERR_LEASE_EXPIRED)
    (map-set rent-escrow-disputes
      { property-id: property-id, tenant: tenant, dispute-id: dispute-id }
      {
        initiator: tx-sender,
        dispute-type: dispute-type,
        escrow-amount: rent-amount,
        arbiter: none,
        status: "pending",
        evidence-hash: evidence-hash,
        submitted-block: stacks-block-height,
        resolved-block: u0,
        resolution: ""
      }
    )
    (var-set dispute-counter dispute-id)
    (ok dispute-id)
  )
)

(define-public (assign-dispute-arbiter (property-id uint) (tenant principal) (dispute-id uint) (arbiter principal))
  (let (
    (dispute (unwrap! (map-get? rent-escrow-disputes { property-id: property-id, tenant: tenant, dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
  )
    (asserts! (or (is-eq tx-sender (get landlord property)) (is-eq tx-sender tenant)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status dispute) "pending") ERR_DISPUTE_NOT_ACTIVE)
    (asserts! (not (is-eq arbiter (get landlord property))) ERR_INVALID_ARBITER)
    (asserts! (not (is-eq arbiter tenant)) ERR_INVALID_ARBITER)
    (map-set rent-escrow-disputes
      { property-id: property-id, tenant: tenant, dispute-id: dispute-id }
      (merge dispute { arbiter: (some arbiter), status: "arbitration" })
    )
    (ok true)
  )
)

(define-public (resolve-dispute (property-id uint) (tenant principal) (dispute-id uint) (favor-tenant bool) (resolution (string-ascii 50)))
  (let (
    (dispute (unwrap! (map-get? rent-escrow-disputes { property-id: property-id, tenant: tenant, dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (arbiter (unwrap! (get arbiter dispute) ERR_INVALID_ARBITER))
  )
    (asserts! (is-eq tx-sender arbiter) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status dispute) "arbitration") ERR_DISPUTE_NOT_ACTIVE)
    (map-set rent-escrow-disputes
      { property-id: property-id, tenant: tenant, dispute-id: dispute-id }
      (merge dispute {
        status: "resolved",
        resolved-block: stacks-block-height,
        resolution: resolution
      })
    )
    (ok favor-tenant)
  )
)

(define-read-only (get-dispute (property-id uint) (tenant principal) (dispute-id uint))
  (map-get? rent-escrow-disputes { property-id: property-id, tenant: tenant, dispute-id: dispute-id })
)

(define-map renewal-incentives
  { property-id: uint }
  {
    enabled: bool,
    tier1-months: uint,
    tier1-discount: uint,
    tier2-months: uint,
    tier2-discount: uint,
    tier3-months: uint,
    tier3-discount: uint,
    min-performance-score: uint
  }
)

(define-map renewal-history
  { property-id: uint, tenant: principal, renewal-count: uint }
  {
    original-start: uint,
    renewed-at: uint,
    discount-applied: uint,
    new-end-block: uint,
    performance-at-renewal: uint
  }
)

(define-public (configure-renewal-incentives 
  (property-id uint) 
  (tier1-months uint) (tier1-discount uint)
  (tier2-months uint) (tier2-discount uint)
  (tier3-months uint) (tier3-discount uint)
  (min-score uint))
  (let ((property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND)))
    (asserts! (is-eq (get landlord property) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (and (<= tier1-discount u100) (<= tier2-discount u100) (<= tier3-discount u100)) ERR_INVALID_TIER_CONFIG)
    (asserts! (and (< tier1-months tier2-months) (< tier2-months tier3-months)) ERR_INVALID_TIER_CONFIG)
    (map-set renewal-incentives
      { property-id: property-id }
      {
        enabled: true,
        tier1-months: tier1-months,
        tier1-discount: tier1-discount,
        tier2-months: tier2-months,
        tier2-discount: tier2-discount,
        tier3-months: tier3-months,
        tier3-discount: tier3-discount,
        min-performance-score: min-score
      }
    )
    (ok true)
  )
)

(define-read-only (calculate-renewal-discount (property-id uint) (tenant principal))
  (match (map-get? renewal-incentives { property-id: property-id })
    incentive (match (map-get? leases { property-id: property-id, tenant: tenant })
      lease (match (map-get? tenant-performance { property-id: property-id, tenant: tenant })
        performance (let (
          (lease-months (/ (- (get end-block lease) (get start-block lease)) u4320))
          (perf-score (get performance-score performance))
        )
          (if (and (get enabled incentive) (>= perf-score (get min-performance-score incentive)))
            (if (>= lease-months (get tier3-months incentive))
              (ok (get tier3-discount incentive))
              (if (>= lease-months (get tier2-months incentive))
                (ok (get tier2-discount incentive))
                (if (>= lease-months (get tier1-months incentive))
                  (ok (get tier1-discount incentive))
                  (ok u0)
                )
              )
            )
            (ok u0)
          )
        )
        (ok u0)
      )
      ERR_LEASE_NOT_FOUND
    )
    (ok u0)
  )
)

(define-public (renew-lease-with-incentive (property-id uint) (renewal-duration uint))
  (let (
    (lease (unwrap! (map-get? leases { property-id: property-id, tenant: tx-sender }) ERR_LEASE_NOT_FOUND))
    (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    (discount-rate (unwrap! (calculate-renewal-discount property-id tx-sender) ERR_RENEWAL_NOT_ELIGIBLE))
    (base-rent (get rent-amount property))
    (discounted-rent (- base-rent (/ (* base-rent discount-rate) u100)))
    (renewal-count (+ u1 (get-renewal-count property-id tx-sender)))
    (perf-score (get performance-score (default-to { payment-streak: u0, late-payments: u0, maintenance-cooperation: u5, tenant-rating: u0, tenant-rating-count: u0, performance-score: u100 } (map-get? tenant-performance { property-id: property-id, tenant: tx-sender }))))
  )
    (asserts! (get active lease) ERR_LEASE_EXPIRED)
    (asserts! (> (get end-block lease) stacks-block-height) ERR_LEASE_EXPIRED)
    (asserts! (<= (- (get end-block lease) stacks-block-height) u4320) ERR_RENEWAL_TOO_EARLY)
    (asserts! (> discount-rate u0) ERR_RENEWAL_NOT_ELIGIBLE)
    (map-set leases
      { property-id: property-id, tenant: tx-sender }
      (merge lease {
        end-block: (+ (get end-block lease) renewal-duration),
        monthly-rent: discounted-rent
      })
    )
    (map-set renewal-history
      { property-id: property-id, tenant: tx-sender, renewal-count: renewal-count }
      {
        original-start: (get start-block lease),
        renewed-at: stacks-block-height,
        discount-applied: discount-rate,
        new-end-block: (+ (get end-block lease) renewal-duration),
        performance-at-renewal: perf-score
      }
    )
    (ok discounted-rent)
  )
)

(define-read-only (get-renewal-incentive-config (property-id uint))
  (map-get? renewal-incentives { property-id: property-id })
)

(define-read-only (get-renewal-record (property-id uint) (tenant principal) (renewal-count uint))
  (map-get? renewal-history { property-id: property-id, tenant: tenant, renewal-count: renewal-count })
)

(define-read-only (get-renewal-count (property-id uint) (tenant principal))
  (match (map-get? renewal-history { property-id: property-id, tenant: tenant, renewal-count: u1 })
    record u1
    u0
  )
)