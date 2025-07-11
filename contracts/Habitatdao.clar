(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPERTY_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_VOTING_ENDED (err u105))
(define-constant ERR_NOT_MEMBER (err u106))
(define-constant ERR_ALREADY_MEMBER (err u107))
(define-constant ERR_INVALID_AMOUNT (err u108))
(define-constant ERR_PROPERTY_ALREADY_EXISTS (err u109))
(define-constant ERR_PROPERTY_NOT_RENTABLE (err u110))
(define-constant ERR_TENANT_NOT_FOUND (err u111))
(define-constant ERR_RENTAL_ALREADY_ACTIVE (err u112))
(define-constant ERR_RENTAL_NOT_ACTIVE (err u113))
(define-constant ERR_RENT_NOT_DUE (err u114))
(define-constant ERR_INVALID_RENT_AMOUNT (err u115))
(define-constant ERR_TENANT_ALREADY_EXISTS (err u116))

(define-data-var next-property-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var next-rental-id uint u1)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var total-rental-income uint u0)

(define-map members principal bool)
(define-map member-shares principal uint)
(define-map member-contributions principal uint)

(define-map properties
  uint
  {
    address: (string-ascii 100),
    price: uint,
    owner: principal,
    is-available: bool,
    total-shares: uint,
    created-at: uint
  }
)

(define-map property-shareholders
  { property-id: uint, member: principal }
  uint
)

(define-map proposals
  uint
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    property-id: uint,
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool,
    proposal-type: (string-ascii 20)
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  bool
)

(define-map rental-contracts
  uint
  {
    property-id: uint,
    tenant: principal,
    monthly-rent: uint,
    security-deposit: uint,
    lease-start: uint,
    lease-end: uint,
    is-active: bool,
    total-rent-paid: uint,
    last-payment-block: uint,
    next-payment-due: uint
  }
)

(define-map property-rentals
  uint
  {
    is-rentable: bool,
    current-rental-id: (optional uint),
    total-rental-income: uint,
    rental-history-count: uint
  }
)

(define-map tenant-profiles
  principal
  {
    total-rentals: uint,
    total-rent-paid: uint,
    current-rentals: uint,
    reputation-score: uint,
    last-rental-end: uint
  }
)

(define-map rental-payments
  { rental-id: uint, payment-number: uint }
  {
    amount: uint,
    payment-block: uint,
    late-fee: uint,
    is-late: bool
  }
)

(define-map shareholder-earnings
  { property-id: uint, member: principal }
  uint
)

(define-public (join-dao (contribution uint))
  (let
    (
      (sender tx-sender)
      (is-member (default-to false (map-get? members sender)))
    )
    (asserts! (not is-member) ERR_ALREADY_MEMBER)
    (asserts! (> contribution u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? contribution sender (as-contract tx-sender)))
    (map-set members sender true)
    (map-set member-contributions sender contribution)
    (map-set member-shares sender (/ contribution u1000))
    (var-set total-members (+ (var-get total-members) u1))
    (var-set treasury-balance (+ (var-get treasury-balance) contribution))
    (ok true)
  )
)

(define-public (add-property (address (string-ascii 100)) (price uint))
  (let
    (
      (property-id (var-get next-property-id))
      (sender tx-sender)
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (map-set properties property-id
      {
        address: address,
        price: price,
        owner: sender,
        is-available: true,
        total-shares: u0,
        created-at: stacks-block-height
      }
    )
    (map-set property-rentals property-id
      {
        is-rentable: true,
        current-rental-id: none,
        total-rental-income: u0,
        rental-history-count: u0
      }
    )
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (invest-in-property (property-id uint) (amount uint))
  (let
    (
      (sender tx-sender)
      (property (unwrap! (map-get? properties property-id) ERR_PROPERTY_NOT_FOUND))
      (current-shares (default-to u0 (map-get? property-shareholders { property-id: property-id, member: sender })))
      (new-shares (/ amount u1000))
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (get is-available property) ERR_PROPERTY_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (map-set property-shareholders 
      { property-id: property-id, member: sender }
      (+ current-shares new-shares)
    )
    (map-set properties property-id
      (merge property { total-shares: (+ (get total-shares property) new-shares) })
    )
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok new-shares)
  )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (property-id uint) (proposal-type (string-ascii 20)))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (sender tx-sender)
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (map-set proposals proposal-id
      {
        title: title,
        description: description,
        property-id: property-id,
        proposer: sender,
        votes-for: u0,
        votes-against: u0,
        end-block: (+ stacks-block-height u144),
        executed: false,
        proposal-type: proposal-type
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (sender tx-sender)
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (has-voted (is-some (map-get? proposal-votes { proposal-id: proposal-id, voter: sender })))
      (member-voting-power (default-to u0 (map-get? member-shares sender)))
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (not has-voted) ERR_ALREADY_VOTED)
    (asserts! (< stacks-block-height (get end-block proposal)) ERR_VOTING_ENDED)
    (map-set proposal-votes { proposal-id: proposal-id, voter: sender } vote-for)
    (if vote-for
      (map-set proposals proposal-id
        (merge proposal { votes-for: (+ (get votes-for proposal) member-voting-power) })
      )
      (map-set proposals proposal-id
        (merge proposal { votes-against: (+ (get votes-against proposal) member-voting-power) })
      )
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (sender tx-sender)
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (>= stacks-block-height (get end-block proposal)) ERR_VOTING_ENDED)
    (asserts! (not (get executed proposal)) ERR_PROPOSAL_NOT_FOUND)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_NOT_AUTHORIZED)
    (map-set proposals proposal-id
      (merge proposal { executed: true })
    )
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (let
    (
      (sender tx-sender)
      (member-contribution (default-to u0 (map-get? member-contributions sender)))
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (>= member-contribution amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= (var-get treasury-balance) amount) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender sender)))
    (map-set member-contributions sender (- member-contribution amount))
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-public (create-rental-contract (property-id uint) (tenant principal) (monthly-rent uint) (security-deposit uint) (lease-duration-blocks uint))
  (let
    (
      (rental-id (var-get next-rental-id))
      (sender tx-sender)
      (property (unwrap! (map-get? properties property-id) ERR_PROPERTY_NOT_FOUND))
      (property-rental (unwrap! (map-get? property-rentals property-id) ERR_PROPERTY_NOT_FOUND))
      (tenant-profile (default-to
        {
          total-rentals: u0,
          total-rent-paid: u0,
          current-rentals: u0,
          reputation-score: u100,
          last-rental-end: u0
        }
        (map-get? tenant-profiles tenant)
      ))
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (get is-available property) ERR_PROPERTY_NOT_RENTABLE)
    (asserts! (get is-rentable property-rental) ERR_PROPERTY_NOT_RENTABLE)
    (asserts! (is-none (get current-rental-id property-rental)) ERR_RENTAL_ALREADY_ACTIVE)
    (asserts! (> monthly-rent u0) ERR_INVALID_RENT_AMOUNT)
    (asserts! (> security-deposit u0) ERR_INVALID_AMOUNT)
    (asserts! (> lease-duration-blocks u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? security-deposit tenant (as-contract tx-sender)))
    (map-set rental-contracts rental-id
      {
        property-id: property-id,
        tenant: tenant,
        monthly-rent: monthly-rent,
        security-deposit: security-deposit,
        lease-start: stacks-block-height,
        lease-end: (+ stacks-block-height lease-duration-blocks),
        is-active: true,
        total-rent-paid: u0,
        last-payment-block: u0,
        next-payment-due: (+ stacks-block-height u144)
      }
    )
    (map-set property-rentals property-id
      (merge property-rental 
        { 
          current-rental-id: (some rental-id),
          rental-history-count: (+ (get rental-history-count property-rental) u1)
        }
      )
    )
    (map-set tenant-profiles tenant
      (merge tenant-profile
        {
          total-rentals: (+ (get total-rentals tenant-profile) u1),
          current-rentals: (+ (get current-rentals tenant-profile) u1)
        }
      )
    )
    (var-set next-rental-id (+ rental-id u1))
    (ok rental-id)
  )
)

(define-public (pay-rent (rental-id uint))
  (let
    (
      (sender tx-sender)
      (rental (unwrap! (map-get? rental-contracts rental-id) ERR_TENANT_NOT_FOUND))
      (property-rental (unwrap! (map-get? property-rentals (get property-id rental)) ERR_PROPERTY_NOT_FOUND))
      (payment-number (+ (/ (get total-rent-paid rental) (get monthly-rent rental)) u1))
      (is-late (> stacks-block-height (get next-payment-due rental)))
      (late-fee (if is-late (/ (get monthly-rent rental) u10) u0))
      (total-payment (+ (get monthly-rent rental) late-fee))
    )
    (asserts! (is-eq sender (get tenant rental)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active rental) ERR_RENTAL_NOT_ACTIVE)
    (asserts! (<= stacks-block-height (get lease-end rental)) ERR_RENTAL_NOT_ACTIVE)
    (try! (stx-transfer? total-payment sender (as-contract tx-sender)))
    (map-set rental-payments 
      { rental-id: rental-id, payment-number: payment-number }
      {
        amount: total-payment,
        payment-block: stacks-block-height,
        late-fee: late-fee,
        is-late: is-late
      }
    )
    (map-set rental-contracts rental-id
      (merge rental
        {
          total-rent-paid: (+ (get total-rent-paid rental) total-payment),
          last-payment-block: stacks-block-height,
          next-payment-due: (+ stacks-block-height u144)
        }
      )
    )
    (map-set property-rentals (get property-id rental)
      (merge property-rental
        {
          total-rental-income: (+ (get total-rental-income property-rental) total-payment)
        }
      )
    )
    (var-set total-rental-income (+ (var-get total-rental-income) total-payment))
    (try! (distribute-rent-to-shareholders (get property-id rental) total-payment))
    (ok true)
  )
)

(define-public (terminate-rental-contract (rental-id uint))
  (let
    (
      (sender tx-sender)
      (rental (unwrap! (map-get? rental-contracts rental-id) ERR_TENANT_NOT_FOUND))
      (property-rental (unwrap! (map-get? property-rentals (get property-id rental)) ERR_PROPERTY_NOT_FOUND))
      (tenant-profile (unwrap! (map-get? tenant-profiles (get tenant rental)) ERR_TENANT_NOT_FOUND))
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (get is-active rental) ERR_RENTAL_NOT_ACTIVE)
    (try! (as-contract (stx-transfer? (get security-deposit rental) tx-sender (get tenant rental))))
    (map-set rental-contracts rental-id
      (merge rental { is-active: false })
    )
    (map-set property-rentals (get property-id rental)
      (merge property-rental { current-rental-id: none })
    )
    (map-set tenant-profiles (get tenant rental)
      (merge tenant-profile
        {
          current-rentals: (- (get current-rentals tenant-profile) u1),
          last-rental-end: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-private (distribute-rent-to-shareholders (property-id uint) (rent-amount uint))
  (let
    (
      (property (unwrap! (map-get? properties property-id) ERR_PROPERTY_NOT_FOUND))
      (total-shares (get total-shares property))
    )
    (if (> total-shares u0)
      (distribute-to-all-shareholders property-id rent-amount total-shares)
      (ok true)
    )
  )
)

(define-private (distribute-to-all-shareholders (property-id uint) (rent-amount uint) (total-shares uint))
  (let
    (
      (distribution-per-share (/ rent-amount total-shares))
    )
    (var-set treasury-balance (+ (var-get treasury-balance) rent-amount))
    (ok true)
  )
)

(define-public (claim-rental-earnings (property-id uint))
  (let
    (
      (sender tx-sender)
      (shareholder-shares (get-property-shares property-id sender))
      (current-earnings (default-to u0 (map-get? shareholder-earnings { property-id: property-id, member: sender })))
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (> shareholder-shares u0) ERR_NOT_AUTHORIZED)
    (asserts! (> current-earnings u0) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? current-earnings tx-sender sender)))
    (map-set shareholder-earnings { property-id: property-id, member: sender } u0)
    (ok current-earnings)
  )
)

(define-public (update-tenant-reputation (tenant principal) (new-score uint))
  (let
    (
      (sender tx-sender)
      (tenant-profile (unwrap! (map-get? tenant-profiles tenant) ERR_TENANT_NOT_FOUND))
    )
    (asserts! (is-member-check sender) ERR_NOT_MEMBER)
    (asserts! (<= new-score u100) ERR_INVALID_AMOUNT)
    (map-set tenant-profiles tenant
      (merge tenant-profile { reputation-score: new-score })
    )
    (ok true)
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? properties property-id)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-member-shares (member principal))
  (default-to u0 (map-get? member-shares member))
)

(define-read-only (get-member-contribution (member principal))
  (default-to u0 (map-get? member-contributions member))
)

(define-read-only (get-property-shares (property-id uint) (member principal))
  (default-to u0 (map-get? property-shareholders { property-id: property-id, member: member }))
)

(define-read-only (is-member (member principal))
  (default-to false (map-get? members member))
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-total-members)
  (var-get total-members)
)

(define-read-only (get-next-property-id)
  (var-get next-property-id)
)

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id)
)

(define-read-only (get-rental-contract (rental-id uint))
  (map-get? rental-contracts rental-id)
)

(define-read-only (get-property-rental-info (property-id uint))
  (map-get? property-rentals property-id)
)

(define-read-only (get-tenant-profile (tenant principal))
  (map-get? tenant-profiles tenant)
)

(define-read-only (get-rental-payment (rental-id uint) (payment-number uint))
  (map-get? rental-payments { rental-id: rental-id, payment-number: payment-number })
)

(define-read-only (get-shareholder-earnings (property-id uint) (member principal))
  (default-to u0 (map-get? shareholder-earnings { property-id: property-id, member: member }))
)

(define-read-only (get-total-rental-income)
  (var-get total-rental-income)
)

(define-read-only (get-next-rental-id)
  (var-get next-rental-id)
)

(define-private (is-member-check (member principal))
  (default-to false (map-get? members member))
)