;; PropertyInsurance Contract - Community-funded insurance pools for property protection
;; Enables shareholders to contribute to insurance pools for collective risk mitigation

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_PROPERTY_NOT_FOUND (err u301))
(define-constant ERR_POOL_NOT_FOUND (err u302))
(define-constant ERR_CLAIM_NOT_FOUND (err u303))
(define-constant ERR_INSUFFICIENT_FUNDS (err u304))
(define-constant ERR_INVALID_AMOUNT (err u305))
(define-constant ERR_NOT_MEMBER (err u306))
(define-constant ERR_ALREADY_CLAIMED (err u307))
(define-constant ERR_CLAIM_EXPIRED (err u308))
(define-constant ERR_INSUFFICIENT_COVERAGE (err u309))
(define-constant ERR_POOL_ALREADY_EXISTS (err u310))

;; Reference to main Habitatdao contract
(define-constant HABITATDAO_CONTRACT .Habitatdao)

;; Insurance constants
(define-constant CLAIM_VOTING_PERIOD u144) ;; ~1 day in blocks
(define-constant MINIMUM_POOL_COVERAGE u100000) ;; 0.1 STX minimum coverage
(define-constant CLAIM_APPROVAL_THRESHOLD u60) ;; 60% approval needed

;; Data variables
(define-data-var next-pool-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var total-insurance-pools uint u0)
(define-data-var total-claims-processed uint u0)

;; Property insurance pools
(define-map insurance-pools uint {
    property-id: uint,
    total-coverage: uint,
    total-contributions: uint,
    active-contributors: uint,
    claims-paid-out: uint,
    pool-created-at: uint,
    is-active: bool,
    coverage-ratio: uint
})

;; Member contributions to insurance pools
(define-map pool-contributions { pool-id: uint, contributor: principal } {
    contribution-amount: uint,
    coverage-share: uint,
    last-contribution: uint,
    total-claims-received: uint
})

;; Insurance claims
(define-map insurance-claims uint {
    pool-id: uint,
    claimant: principal,
    damage-description: (string-ascii 300),
    claim-amount: uint,
    evidence-hash: (string-ascii 64),
    filed-at: uint,
    voting-ends-at: uint,
    votes-for: uint,
    votes-against: uint,
    total-voters: uint,
    status: uint, ;; 1: pending, 2: approved, 3: rejected, 4: paid
    approved-amount: uint,
    paid-at: uint
})

;; Claim voting records
(define-map claim-votes { claim-id: uint, voter: principal } {
    vote: bool,
    voting-power: uint,
    voted-at: uint
})

;; Property pool lookup
(define-map property-pools uint uint)

;; Pool contributor lists
(define-map pool-contributors uint (list 50 principal))

;; Member insurance statistics
(define-map member-insurance-stats principal {
    total-contributions: uint,
    pools-contributed: uint,
    claims-filed: uint,
    claims-approved: uint,
    total-payouts: uint
})

;; Create insurance pool for a property
(define-public (create-insurance-pool (property-id uint) (initial-coverage uint))
    (let (
        (pool-id (var-get next-pool-id))
        (property-data (unwrap! (contract-call? HABITATDAO_CONTRACT get-property property-id) ERR_PROPERTY_NOT_FOUND))
        (member-shares (contract-call? HABITATDAO_CONTRACT get-property-shares property-id tx-sender))
        (member-stats (default-to 
            { total-contributions: u0, pools-contributed: u0, claims-filed: u0, claims-approved: u0, total-payouts: u0 }
            (map-get? member-insurance-stats tx-sender)))
    )
        ;; Validate member is shareholder and amount
        (asserts! (> member-shares u0) ERR_NOT_MEMBER)
        (asserts! (> initial-coverage u0) ERR_INVALID_AMOUNT)
        (asserts! (>= initial-coverage MINIMUM_POOL_COVERAGE) ERR_INSUFFICIENT_COVERAGE)
        (asserts! (is-none (map-get? property-pools property-id)) ERR_POOL_ALREADY_EXISTS)
        
        ;; Transfer initial coverage to contract
        (try! (stx-transfer? initial-coverage tx-sender (as-contract tx-sender)))
        
        ;; Create insurance pool
        (map-set insurance-pools pool-id {
            property-id: property-id,
            total-coverage: initial-coverage,
            total-contributions: initial-coverage,
            active-contributors: u1,
            claims-paid-out: u0,
            pool-created-at: stacks-block-height,
            is-active: true,
            coverage-ratio: u100
        })
        
        ;; Record contributor
        (map-set pool-contributions { pool-id: pool-id, contributor: tx-sender } {
            contribution-amount: initial-coverage,
            coverage-share: u100,
            last-contribution: stacks-block-height,
            total-claims-received: u0
        })
        
        ;; Initialize contributor list
        (map-set pool-contributors pool-id (list tx-sender))
        
        ;; Link property to pool
        (map-set property-pools property-id pool-id)
        
        ;; Update member stats
        (map-set member-insurance-stats tx-sender
            (merge member-stats {
                total-contributions: (+ (get total-contributions member-stats) initial-coverage),
                pools-contributed: (+ (get pools-contributed member-stats) u1)
            }))
        
        (var-set next-pool-id (+ pool-id u1))
        (var-set total-insurance-pools (+ (var-get total-insurance-pools) u1))
        
        (ok pool-id)
    )
)

;; Contribute to existing insurance pool
(define-public (contribute-to-pool (pool-id uint) (contribution-amount uint))
    (let (
        (pool-data (unwrap! (map-get? insurance-pools pool-id) ERR_POOL_NOT_FOUND))
        (property-id (get property-id pool-data))
        (member-shares (contract-call? HABITATDAO_CONTRACT get-property-shares property-id tx-sender))
        (existing-contribution (default-to 
            { contribution-amount: u0, coverage-share: u0, last-contribution: u0, total-claims-received: u0 }
            (map-get? pool-contributions { pool-id: pool-id, contributor: tx-sender })))
        (contributors (default-to (list) (map-get? pool-contributors pool-id)))
        (is-new-contributor (is-eq (get contribution-amount existing-contribution) u0))
        (new-total-coverage (+ (get total-coverage pool-data) contribution-amount))
        (new-contribution-total (+ (get contribution-amount existing-contribution) contribution-amount))
        (coverage-share (/ (* new-contribution-total u100) new-total-coverage))
    )
        ;; Validate member and amount
        (asserts! (> member-shares u0) ERR_NOT_MEMBER)
        (asserts! (get is-active pool-data) ERR_POOL_NOT_FOUND)
        (asserts! (> contribution-amount u0) ERR_INVALID_AMOUNT)
        
        ;; Transfer contribution to contract
        (try! (stx-transfer? contribution-amount tx-sender (as-contract tx-sender)))
        
        ;; Update pool
        (map-set insurance-pools pool-id
            (merge pool-data {
                total-coverage: new-total-coverage,
                total-contributions: (+ (get total-contributions pool-data) contribution-amount),
                active-contributors: (if is-new-contributor 
                                    (+ (get active-contributors pool-data) u1) 
                                    (get active-contributors pool-data))
            }))
        
        ;; Update contributor record
        (map-set pool-contributions { pool-id: pool-id, contributor: tx-sender }
            (merge existing-contribution {
                contribution-amount: new-contribution-total,
                coverage-share: coverage-share,
                last-contribution: stacks-block-height
            }))
        
        ;; Add to contributors list if new
        (if is-new-contributor
            (map-set pool-contributors pool-id 
                (unwrap! (as-max-len? (append contributors tx-sender) u50) ERR_INSUFFICIENT_FUNDS))
            true
        )
        
        (ok true)
    )
)

;; File insurance claim
(define-public (file-insurance-claim (pool-id uint) (damage-description (string-ascii 300)) (claim-amount uint) (evidence-hash (string-ascii 64)))
    (let (
        (claim-id (var-get next-claim-id))
        (pool-data (unwrap! (map-get? insurance-pools pool-id) ERR_POOL_NOT_FOUND))
        (property-id (get property-id pool-data))
        (member-shares (contract-call? HABITATDAO_CONTRACT get-property-shares property-id tx-sender))
        (member-stats (default-to 
            { total-contributions: u0, pools-contributed: u0, claims-filed: u0, claims-approved: u0, total-payouts: u0 }
            (map-get? member-insurance-stats tx-sender)))
    )
        ;; Validate claimer and amounts
        (asserts! (> member-shares u0) ERR_NOT_MEMBER)
        (asserts! (get is-active pool-data) ERR_POOL_NOT_FOUND)
        (asserts! (> claim-amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= claim-amount (get total-coverage pool-data)) ERR_INSUFFICIENT_COVERAGE)
        
        ;; Create claim
        (map-set insurance-claims claim-id {
            pool-id: pool-id,
            claimant: tx-sender,
            damage-description: damage-description,
            claim-amount: claim-amount,
            evidence-hash: evidence-hash,
            filed-at: stacks-block-height,
            voting-ends-at: (+ stacks-block-height CLAIM_VOTING_PERIOD),
            votes-for: u0,
            votes-against: u0,
            total-voters: u0,
            status: u1,
            approved-amount: u0,
            paid-at: u0
        })
        
        ;; Update member stats
        (map-set member-insurance-stats tx-sender
            (merge member-stats {
                claims-filed: (+ (get claims-filed member-stats) u1)
            }))
        
        (var-set next-claim-id (+ claim-id u1))
        
        (ok claim-id)
    )
)

;; Vote on insurance claim
(define-public (vote-on-claim (claim-id uint) (approve bool))
    (let (
        (claim-data (unwrap! (map-get? insurance-claims claim-id) ERR_CLAIM_NOT_FOUND))
        (pool-data (unwrap! (map-get? insurance-pools (get pool-id claim-data)) ERR_POOL_NOT_FOUND))
        (property-id (get property-id pool-data))
        (voting-power (contract-call? HABITATDAO_CONTRACT get-property-shares property-id tx-sender))
        (has-voted (is-some (map-get? claim-votes { claim-id: claim-id, voter: tx-sender })))
    )
        ;; Validate voter and timing
        (asserts! (> voting-power u0) ERR_NOT_MEMBER)
        (asserts! (not has-voted) ERR_ALREADY_CLAIMED)
        (asserts! (< stacks-block-height (get voting-ends-at claim-data)) ERR_CLAIM_EXPIRED)
        (asserts! (is-eq (get status claim-data) u1) ERR_ALREADY_CLAIMED)
        
        ;; Record vote
        (map-set claim-votes { claim-id: claim-id, voter: tx-sender } {
            vote: approve,
            voting-power: voting-power,
            voted-at: stacks-block-height
        })
        
        ;; Update claim vote tallies
        (if approve
            (map-set insurance-claims claim-id
                (merge claim-data {
                    votes-for: (+ (get votes-for claim-data) voting-power),
                    total-voters: (+ (get total-voters claim-data) u1)
                }))
            (map-set insurance-claims claim-id
                (merge claim-data {
                    votes-against: (+ (get votes-against claim-data) voting-power),
                    total-voters: (+ (get total-voters claim-data) u1)
                }))
        )
        
        (ok true)
    )
)

;; Process claim after voting period
(define-public (process-claim (claim-id uint))
    (let (
        (claim-data (unwrap! (map-get? insurance-claims claim-id) ERR_CLAIM_NOT_FOUND))
        (pool-data (unwrap! (map-get? insurance-pools (get pool-id claim-data)) ERR_POOL_NOT_FOUND))
        (total-votes (+ (get votes-for claim-data) (get votes-against claim-data)))
        (approval-percentage (if (> total-votes u0) (/ (* (get votes-for claim-data) u100) total-votes) u0))
        (is-approved (>= approval-percentage CLAIM_APPROVAL_THRESHOLD))
        (payout-amount (if is-approved (get claim-amount claim-data) u0))
    )
        ;; Validate timing and status
        (asserts! (>= stacks-block-height (get voting-ends-at claim-data)) ERR_CLAIM_EXPIRED)
        (asserts! (is-eq (get status claim-data) u1) ERR_ALREADY_CLAIMED)
        
        ;; Process based on approval
        (if is-approved
            (begin
                ;; Pay out claim
                (asserts! (>= (get total-coverage pool-data) payout-amount) ERR_INSUFFICIENT_FUNDS)
                (try! (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim-data))))
                
                ;; Update pool coverage
                (map-set insurance-pools (get pool-id claim-data)
                    (merge pool-data {
                        total-coverage: (- (get total-coverage pool-data) payout-amount),
                        claims-paid-out: (+ (get claims-paid-out pool-data) payout-amount)
                    }))
                
                ;; Update claim status
                (map-set insurance-claims claim-id
                    (merge claim-data {
                        status: u4,
                        approved-amount: payout-amount,
                        paid-at: stacks-block-height
                    }))
            )
            ;; Reject claim
            (map-set insurance-claims claim-id
                (merge claim-data { status: u3 }))
        )
        
        (var-set total-claims-processed (+ (var-get total-claims-processed) u1))
        
        (ok is-approved)
    )
)

;; Read-only functions
(define-read-only (get-insurance-pool (pool-id uint))
    (map-get? insurance-pools pool-id)
)

(define-read-only (get-property-pool (property-id uint))
    (match (map-get? property-pools property-id)
        pool-id (map-get? insurance-pools pool-id)
        none
    )
)

(define-read-only (get-pool-contribution (pool-id uint) (contributor principal))
    (map-get? pool-contributions { pool-id: pool-id, contributor: contributor })
)

(define-read-only (get-insurance-claim (claim-id uint))
    (map-get? insurance-claims claim-id)
)

(define-read-only (get-claim-vote (claim-id uint) (voter principal))
    (map-get? claim-votes { claim-id: claim-id, voter: voter })
)

(define-read-only (get-pool-contributors (pool-id uint))
    (map-get? pool-contributors pool-id)
)

(define-read-only (get-member-insurance-stats (member principal))
    (map-get? member-insurance-stats member)
)

(define-read-only (get-pool-coverage-info (pool-id uint))
    (match (map-get? insurance-pools pool-id)
        pool-data (some {
            total-coverage: (get total-coverage pool-data),
            active-contributors: (get active-contributors pool-data),
            claims-paid-out: (get claims-paid-out pool-data),
            coverage-utilization: (if (> (get total-contributions pool-data) u0)
                                   (/ (* (get claims-paid-out pool-data) u100) (get total-contributions pool-data))
                                   u0),
            is-active: (get is-active pool-data)
        })
        none
    )
)

(define-read-only (get-contract-stats)
    {
        total-insurance-pools: (var-get total-insurance-pools),
        total-claims-processed: (var-get total-claims-processed),
        next-pool-id: (var-get next-pool-id),
        next-claim-id: (var-get next-claim-id),
        minimum-pool-coverage: MINIMUM_POOL_COVERAGE,
        claim-voting-period: CLAIM_VOTING_PERIOD,
        approval-threshold: CLAIM_APPROVAL_THRESHOLD
    }
)