;; MaintenanceScheduler - Community Maintenance Coordination for HabitatDAO
;; Enables decentralized maintenance scheduling, contractor management, and cost-sharing

;; Constants
(define-constant err-not-authorized u401)
(define-constant err-property-not-found u404)
(define-constant err-request-not-found u402)
(define-constant err-invalid-amount u405)
(define-constant err-not-member u403)
(define-constant err-invalid-status u407)
(define-constant status-pending u1)
(define-constant status-approved u2)
(define-constant status-completed u4)
(define-constant HABITATDAO_CONTRACT .Habitatdao)

;; Data Variables
(define-data-var next-request-id uint u1)
(define-data-var total-maintenance-costs uint u0)

;; Maintenance Requests
(define-map maintenance-requests uint {
    property-id: uint,
    requester: principal,
    title: (string-ascii 100),
    description: (string-ascii 300),
    urgency-level: uint,
    estimated-cost: uint,
    actual-cost: uint,
    contractor: (optional principal),
    status: uint,
    scheduled-date: uint,
    completion-date: uint,
    created-at: uint,
    approval-votes: uint,
    total-votes: uint
})

;; Contractor Registry
(define-map contractors principal {
    name: (string-ascii 50),
    specialties: (string-ascii 200),
    total-jobs: uint,
    registered-at: uint
})

;; Request Voting
(define-map request-votes { request-id: uint, voter: principal } {
    vote: bool,
    voting-power: uint,
    voted-at: uint
})

;; Property Maintenance Stats
(define-map property-maintenance { property-id: uint } {
    total-requests: uint,
    completed-requests: uint,
    total-spent: uint,
    last-maintenance: uint
})

;; Submit maintenance request
(define-public (submit-maintenance-request (property-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (urgency-level uint) (estimated-cost uint))
    (let (
        (request-id (var-get next-request-id))
        (property-data (unwrap! (contract-call? HABITATDAO_CONTRACT get-property property-id) (err err-property-not-found)))
        (member-shares (contract-call? HABITATDAO_CONTRACT get-property-shares property-id tx-sender))
    )
        (asserts! (> member-shares u0) (err err-not-member))
        (asserts! (> estimated-cost u0) (err err-invalid-amount))
        (asserts! (<= urgency-level u5) (err err-invalid-amount))

        ;; Create maintenance request
        (map-set maintenance-requests request-id {
            property-id: property-id,
            requester: tx-sender,
            title: title,
            description: description,
            urgency-level: urgency-level,
            estimated-cost: estimated-cost,
            actual-cost: u0,
            contractor: none,
            status: status-pending,
            scheduled-date: u0,
            completion-date: u0,
            created-at: stacks-block-height,
            approval-votes: u0,
            total-votes: u0
        })

        ;; Update property stats
        (let (
            (prop-stats (default-to 
                { total-requests: u0, completed-requests: u0, total-spent: u0, last-maintenance: u0 }
                (map-get? property-maintenance { property-id: property-id })))
        )
            (map-set property-maintenance { property-id: property-id }
                (merge prop-stats { total-requests: (+ (get total-requests prop-stats) u1) }))
        )

        (var-set next-request-id (+ request-id u1))
        (ok request-id)
    )
)

;; Vote on maintenance request
(define-public (vote-on-request (request-id uint) (approve bool))
    (let (
        (request (unwrap! (map-get? maintenance-requests request-id) (err err-request-not-found)))
        (property-id (get property-id request))
        (voting-power (contract-call? HABITATDAO_CONTRACT get-property-shares property-id tx-sender))
        (has-voted (is-some (map-get? request-votes { request-id: request-id, voter: tx-sender })))
    )
        (asserts! (> voting-power u0) (err err-not-member))
        (asserts! (not has-voted) (err err-not-authorized))
        (asserts! (is-eq (get status request) status-pending) (err err-invalid-status))

        ;; Record vote
        (map-set request-votes { request-id: request-id, voter: tx-sender } {
            vote: approve,
            voting-power: voting-power,
            voted-at: stacks-block-height
        })

        ;; Update request votes
        (map-set maintenance-requests request-id
            (merge request {
                approval-votes: (if approve (+ (get approval-votes request) voting-power) (get approval-votes request)),
                total-votes: (+ (get total-votes request) voting-power)
            }))

        (ok true)
    )
)

;; Register as contractor
(define-public (register-contractor (name (string-ascii 50)) (specialties (string-ascii 200)))
    (begin
        (asserts! (is-none (map-get? contractors tx-sender)) (err err-not-authorized))
        
        (map-set contractors tx-sender {
            name: name,
            specialties: specialties,
            total-jobs: u0,
            registered-at: stacks-block-height
        })
        (ok true)
    )
)

;; Assign contractor to approved request
(define-public (assign-contractor (request-id uint) (contractor principal) (scheduled-date uint))
    (let (
        (request (unwrap! (map-get? maintenance-requests request-id) (err err-request-not-found)))
        (contractor-data (unwrap! (map-get? contractors contractor) (err err-not-authorized)))
        (property-id (get property-id request))
        (member-shares (contract-call? HABITATDAO_CONTRACT get-property-shares property-id tx-sender))
        (approval-ratio (if (> (get total-votes request) u0) 
                          (/ (* (get approval-votes request) u100) (get total-votes request)) u0))
    )
        (asserts! (> member-shares u0) (err err-not-member))
        (asserts! (is-eq (get status request) status-pending) (err err-invalid-status))
        (asserts! (>= approval-ratio u60) (err err-not-authorized))
        (asserts! (> scheduled-date stacks-block-height) (err err-invalid-amount))

        ;; Update request with contractor
        (map-set maintenance-requests request-id
            (merge request {
                contractor: (some contractor),
                status: status-approved,
                scheduled-date: scheduled-date
            }))

        (ok true)
    )
)

;; Complete maintenance and process payment
(define-public (complete-maintenance (request-id uint) (actual-cost uint))
    (let (
        (request (unwrap! (map-get? maintenance-requests request-id) (err err-request-not-found)))
        (contractor-addr (unwrap! (get contractor request) (err err-not-authorized)))
        (property-id (get property-id request))
    )
        (asserts! (is-eq tx-sender contractor-addr) (err err-not-authorized))
        (asserts! (is-eq (get status request) status-approved) (err err-invalid-status))
        (asserts! (> actual-cost u0) (err err-invalid-amount))

        ;; Update request as completed
        (map-set maintenance-requests request-id
            (merge request {
                status: status-completed,
                actual-cost: actual-cost,
                completion-date: stacks-block-height
            }))

        ;; Update property stats
        (let (
            (prop-stats (default-to 
                { total-requests: u0, completed-requests: u0, total-spent: u0, last-maintenance: u0 }
                (map-get? property-maintenance { property-id: property-id })))
        )
            (map-set property-maintenance { property-id: property-id }
                (merge prop-stats {
                    completed-requests: (+ (get completed-requests prop-stats) u1),
                    total-spent: (+ (get total-spent prop-stats) actual-cost),
                    last-maintenance: stacks-block-height
                }))
        )

        (var-set total-maintenance-costs (+ (var-get total-maintenance-costs) actual-cost))
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-maintenance-request (request-id uint))
    (map-get? maintenance-requests request-id))

(define-read-only (get-contractor-info (contractor principal))
    (map-get? contractors contractor))

(define-read-only (get-property-maintenance-stats (property-id uint))
    (map-get? property-maintenance { property-id: property-id }))

(define-read-only (get-maintenance-stats)
    {
        next-request-id: (var-get next-request-id),
        total-maintenance-costs: (var-get total-maintenance-costs)
    })
