;; GridFluxon - Decentralized Educational Impact Network

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-amount (err u105))

;; Data Variables
(define-data-var total-staked uint u0)
(define-data-var total-initiatives uint u0)
(define-data-var platform-fee-percentage uint u2) ;; 2% platform fee

;; Data Maps
(define-map educational-initiatives
  { initiative-id: uint }
  {
    creator: principal,
    name: (string-ascii 50),
    target-amount: uint,
    current-funding: uint,
    total-impact-score: uint,
    active: bool,
    created-at: uint
  }
)

(define-map student-progress
  { student: principal, initiative-id: uint }
  {
    milestones-completed: uint,
    impact-score: uint,
    tokens-earned: uint,
    last-updated: uint
  }
)

(define-map validator-stakes
  { validator: principal }
  {
    staked-amount: uint,
    reputation-score: uint,
    validations-completed: uint
  }
)

(define-map funding-pools
  { initiative-id: uint }
  {
    available-funds: uint,
    distributed-funds: uint,
    contributor-count: uint
  }
)

(define-map contributor-records
  { contributor: principal, initiative-id: uint }
  {
    amount-contributed: uint,
    contribution-date: uint
  }
)

;; Private Functions
(define-private (calculate-dynamic-allocation (impact-score uint) (total-impact uint))
  (if (is-eq total-impact u0)
    u0
    (/ (* impact-score u100) total-impact)
  )
)

;; Public Functions

;; Create new educational initiative
(define-public (create-initiative (name (string-ascii 50)) (target-amount uint))
  (let
    (
      (initiative-id (+ (var-get total-initiatives) u1))
    )
    (asserts! (> target-amount u0) err-invalid-amount)
    (map-set educational-initiatives
      { initiative-id: initiative-id }
      {
        creator: tx-sender,
        name: name,
        target-amount: target-amount,
        current-funding: u0,
        total-impact-score: u0,
        active: true,
        created-at: block-height
      }
    )
    (map-set funding-pools
      { initiative-id: initiative-id }
      {
        available-funds: u0,
        distributed-funds: u0,
        contributor-count: u0
      }
    )
    (var-set total-initiatives initiative-id)
    (ok initiative-id)
  )
)

;; Contribute funds to an initiative
(define-public (contribute-to-initiative (initiative-id uint) (amount uint))
  (let
    (
      (initiative (unwrap! (map-get? educational-initiatives { initiative-id: initiative-id }) err-not-found))
      (pool (unwrap! (map-get? funding-pools { initiative-id: initiative-id }) err-not-found))
      (existing-contribution (default-to 
        { amount-contributed: u0, contribution-date: u0 }
        (map-get? contributor-records { contributor: tx-sender, initiative-id: initiative-id })
      ))
    )
    (asserts! (get active initiative) err-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Update initiative funding
    (map-set educational-initiatives
      { initiative-id: initiative-id }
      (merge initiative { current-funding: (+ (get current-funding initiative) amount) })
    )
    
    ;; Update funding pool
    (map-set funding-pools
      { initiative-id: initiative-id }
      {
        available-funds: (+ (get available-funds pool) amount),
        distributed-funds: (get distributed-funds pool),
        contributor-count: (if (is-eq (get amount-contributed existing-contribution) u0)
          (+ (get contributor-count pool) u1)
          (get contributor-count pool)
        )
      }
    )
    
    ;; Record contribution
    (map-set contributor-records
      { contributor: tx-sender, initiative-id: initiative-id }
      {
        amount-contributed: (+ (get amount-contributed existing-contribution) amount),
        contribution-date: block-height
      }
    )
    
    (ok true)
  )
)

;; Record student milestone completion
(define-public (record-milestone (initiative-id uint) (student principal) (impact-score-increase uint))
  (let
    (
      (initiative (unwrap! (map-get? educational-initiatives { initiative-id: initiative-id }) err-not-found))
      (progress (default-to
        { milestones-completed: u0, impact-score: u0, tokens-earned: u0, last-updated: u0 }
        (map-get? student-progress { student: student, initiative-id: initiative-id })
      ))
    )
    (asserts! (get active initiative) err-not-found)
    
    ;; Update student progress
    (map-set student-progress
      { student: student, initiative-id: initiative-id }
      {
        milestones-completed: (+ (get milestones-completed progress) u1),
        impact-score: (+ (get impact-score progress) impact-score-increase),
        tokens-earned: (get tokens-earned progress),
        last-updated: block-height
      }
    )
    
    ;; Update initiative total impact
    (map-set educational-initiatives
      { initiative-id: initiative-id }
      (merge initiative { 
        total-impact-score: (+ (get total-impact-score initiative) impact-score-increase)
      })
    )
    
    (ok true)
  )
)

;; Stake tokens to become a validator
(define-public (stake-as-validator (amount uint))
  (let
    (
      (current-stake (default-to
        { staked-amount: u0, reputation-score: u100, validations-completed: u0 }
        (map-get? validator-stakes { validator: tx-sender })
      ))
    )
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set validator-stakes
      { validator: tx-sender }
      {
        staked-amount: (+ (get staked-amount current-stake) amount),
        reputation-score: (get reputation-score current-stake),
        validations-completed: (get validations-completed current-stake)
      }
    )
    
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok true)
  )
)

;; Distribute rewards to student for teaching
(define-public (reward-student-teaching (initiative-id uint) (student principal) (reward-amount uint))
  (let
    (
      (initiative (unwrap! (map-get? educational-initiatives { initiative-id: initiative-id }) err-not-found))
      (pool (unwrap! (map-get? funding-pools { initiative-id: initiative-id }) err-not-found))
      (progress (unwrap! (map-get? student-progress { student: student, initiative-id: initiative-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator initiative)) err-unauthorized)
    (asserts! (>= (get available-funds pool) reward-amount) err-insufficient-balance)
    
    ;; Update student tokens earned
    (map-set student-progress
      { student: student, initiative-id: initiative-id }
      (merge progress { 
        tokens-earned: (+ (get tokens-earned progress) reward-amount)
      })
    )
    
    ;; Update pool funds
    (map-set funding-pools
      { initiative-id: initiative-id }
      {
        available-funds: (- (get available-funds pool) reward-amount),
        distributed-funds: (+ (get distributed-funds pool) reward-amount),
        contributor-count: (get contributor-count pool)
      }
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-initiative (initiative-id uint))
  (map-get? educational-initiatives { initiative-id: initiative-id })
)

(define-read-only (get-student-progress (student principal) (initiative-id uint))
  (map-get? student-progress { student: student, initiative-id: initiative-id })
)

(define-read-only (get-validator-info (validator principal))
  (map-get? validator-stakes { validator: validator })
)

(define-read-only (get-funding-pool (initiative-id uint))
  (map-get? funding-pools { initiative-id: initiative-id })
)

(define-read-only (get-total-staked)
  (ok (var-get total-staked))
)

(define-read-only (get-total-initiatives)
  (ok (var-get total-initiatives))
)

(define-read-only (get-contribution (contributor principal) (initiative-id uint))
  (map-get? contributor-records { contributor: contributor, initiative-id: initiative-id })
)