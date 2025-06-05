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

(define-data-var next-property-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)

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

(define-private (is-member-check (member principal))
  (default-to false (map-get? members member))
)