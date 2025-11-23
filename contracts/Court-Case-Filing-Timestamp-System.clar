(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-case-exists (err u101))
(define-constant err-invalid-hash (err u102))
(define-constant err-case-not-found (err u103))
(define-constant err-not-registered (err u104))
(define-constant err-duplicate (err u105))
(define-constant MAX_ENTRIES u100)

(define-data-var system-status bool true)
(define-data-var total-cases uint u0)

(define-map authorized-entities principal bool)
(define-map case-records 
    { case-id: (string-ascii 32) }
    {
        timestamp: uint,
        document-hash: (buff 32),
        filed-by: principal,
        jurisdiction: (string-ascii 32),
        case-type: (string-ascii 32),
        status: (string-ascii 16)
    }
)

(define-map case-history
    { case-id: (string-ascii 32), sequence: uint }
    {
        doc-hash: (buff 32),
        submitter: principal,
        timestamp: uint
    }
)

(define-map case-index
    { case-id: (string-ascii 32) }
    { count: uint }
)

(define-private (check-duplicate (case-id (string-ascii 32)) (doc-hash (buff 32)) (limit uint))
    (let ((existing-entry (map-get? case-history {case-id: case-id, sequence: limit})))
        (if (is-some existing-entry)
            (if (is-eq (get doc-hash (unwrap! existing-entry err-duplicate)) doc-hash)
                err-duplicate
                (ok true))
            (ok true))))

(define-public (file-case 
    (case-id (string-ascii 32))
    (document-hash (buff 32))
    (jurisdiction (string-ascii 32))
    (case-type (string-ascii 32)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (current-count (default-to u0 (get count (map-get? case-index {case-id: case-id})))))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (< current-count MAX_ENTRIES) err-case-exists)
        (asserts! (> (len document-hash) u0) err-invalid-hash)
        (try! (check-duplicate case-id document-hash current-count))
        (map-set case-history 
            {case-id: case-id, sequence: current-count}
            {doc-hash: document-hash, submitter: caller, timestamp: current-time})
        (map-set case-index 
            {case-id: case-id} 
            {count: (+ current-count u1)})
        (if (is-eq current-count u0)
            (begin
                (unwrap-panic (update-search-indices case-id jurisdiction case-type status-pending))
                (ok current-time))
            (ok current-time))))
(define-public (register-entity (entity principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (ok (map-set authorized-entities entity true))))

(define-public (remove-entity (entity principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (ok (map-delete authorized-entities entity))))

(define-read-only (is-entity-authorized (entity principal))
    (default-to false (map-get? authorized-entities entity)))

(define-read-only (get-case-details (case-id (string-ascii 32)) (sequence uint))
    (map-get? case-history {case-id: case-id, sequence: sequence}))

(define-read-only (get-case-count (case-id (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-index {case-id: case-id}))))

(define-read-only (get-total-cases)
    (ok (var-get total-cases)))

(define-public (toggle-system)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (ok (var-set system-status (not (var-get system-status))))))

(define-constant status-pending "pending")
(define-constant status-active "active") 
(define-constant status-closed "closed")
(define-constant status-appealed "appealed")
(define-constant err-invalid-status (err u106))

(define-map case-status-history
    { case-id: (string-ascii 32), status-sequence: uint }
    {
        status: (string-ascii 16),
        updated-by: principal,
        timestamp: uint,
        notes: (string-ascii 256)
    }
)

(define-map case-status-index
    { case-id: (string-ascii 32) }
    { current-status: (string-ascii 16), status-count: uint }
)

(define-private (is-valid-status (status (string-ascii 16)))
    (or (is-eq status status-pending)
        (or (is-eq status status-active)
            (or (is-eq status status-closed)
                (is-eq status status-appealed)))))

(define-public (update-case-status 
    (case-id (string-ascii 32))
    (new-status (string-ascii 16))
    (notes (string-ascii 256)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (status-info (default-to {current-status: status-pending, status-count: u0} 
                                 (map-get? case-status-index {case-id: case-id})))
         (current-count (get status-count status-info)))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (is-valid-status new-status) err-invalid-status)
        (asserts! (> (get-case-count case-id) u0) err-case-not-found)
        (map-set case-status-history
            {case-id: case-id, status-sequence: current-count}
            {status: new-status, updated-by: caller, timestamp: current-time, notes: notes})
        (map-set case-status-index
            {case-id: case-id}
            {current-status: new-status, status-count: (+ current-count u1)})
        (unwrap-panic (send-case-notifications case-id alert-type-status-change (concat "Status changed to: " new-status)))
        (ok current-time)))

(define-read-only (get-current-case-status (case-id (string-ascii 32)))
    (get current-status (default-to {current-status: status-pending, status-count: u0}
                                   (map-get? case-status-index {case-id: case-id}))))

(define-read-only (get-case-status-history (case-id (string-ascii 32)) (status-sequence uint))
    (map-get? case-status-history {case-id: case-id, status-sequence: status-sequence}))

(define-read-only (get-case-status-count (case-id (string-ascii 32)))
    (get status-count (default-to {current-status: status-pending, status-count: u0}
                                 (map-get? case-status-index {case-id: case-id}))))

                                 (define-constant err-doc-not-found (err u107))
                                 
(define-constant err-already-verified (err u108))
(define-constant err-verification-failed (err u109))

(define-map document-metadata
    { doc-hash: (buff 32) }
    {
        case-id: (string-ascii 32),
        doc-type: (string-ascii 32),
        file-size: uint,
        uploader: principal,
        upload-timestamp: uint,
        is-verified: bool,
        verification-timestamp: (optional uint),
        verified-by: (optional principal)
    }
)

(define-map case-documents
    { case-id: (string-ascii 32), doc-index: uint }
    { doc-hash: (buff 32) }
)

(define-map case-doc-count
    { case-id: (string-ascii 32) }
    { count: uint }
)

(define-public (register-document
    (case-id (string-ascii 32))
    (document-hash (buff 32))
    (doc-type (string-ascii 32))
    (file-size uint))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (doc-count (default-to u0 (get count (map-get? case-doc-count {case-id: case-id})))))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (> (len document-hash) u0) err-invalid-hash)
        (asserts! (is-none (map-get? document-metadata {doc-hash: document-hash})) err-duplicate)
        (map-set document-metadata
            {doc-hash: document-hash}
            {case-id: case-id, doc-type: doc-type, file-size: file-size,
             uploader: caller, upload-timestamp: current-time, is-verified: false,
             verification-timestamp: none, verified-by: none})
        (map-set case-documents
            {case-id: case-id, doc-index: doc-count}
            {doc-hash: document-hash})
        (map-set case-doc-count
            {case-id: case-id}
            {count: (+ doc-count u1)})
        (unwrap-panic (send-case-notifications case-id alert-type-document-added (concat "New document added: " doc-type)))
        (ok current-time)))

(define-public (verify-document (document-hash (buff 32)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (doc-info (unwrap! (map-get? document-metadata {doc-hash: document-hash}) err-doc-not-found)))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (not (get is-verified doc-info)) err-already-verified)
        (map-set document-metadata
            {doc-hash: document-hash}
            (merge doc-info {is-verified: true, 
                           verification-timestamp: (some current-time),
                           verified-by: (some caller)}))
        (unwrap-panic (send-case-notifications (get case-id doc-info) alert-type-document-verified "Document verification completed"))
        (ok current-time)))

(define-read-only (get-document-info (document-hash (buff 32)))
    (map-get? document-metadata {doc-hash: document-hash}))

(define-read-only (get-case-document (case-id (string-ascii 32)) (doc-index uint))
    (map-get? case-documents {case-id: case-id, doc-index: doc-index}))

(define-read-only (get-case-document-count (case-id (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-doc-count {case-id: case-id}))))

(define-read-only (is-document-verified (document-hash (buff 32)))
    (match (map-get? document-metadata {doc-hash: document-hash})
        doc-info (get is-verified doc-info)
        false))

(define-constant err-invalid-search (err u110))
(define-constant max-search-results u50)

(define-map jurisdiction-cases
    { jurisdiction: (string-ascii 32), jurisdiction-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map jurisdiction-index
    { jurisdiction: (string-ascii 32) }
    { count: uint }
)

(define-map case-type-cases
    { case-type: (string-ascii 32), type-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map case-type-index
    { case-type: (string-ascii 32) }
    { count: uint }
)

(define-map status-cases
    { status: (string-ascii 16), status-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map status-search-index
    { status: (string-ascii 16) }
    { count: uint }
)

(define-private (update-search-indices (case-id (string-ascii 32)) (jurisdiction (string-ascii 32)) (case-type (string-ascii 32)) (status (string-ascii 16)))
    (let
        ((jurisdiction-count (default-to u0 (get count (map-get? jurisdiction-index {jurisdiction: jurisdiction}))))
         (type-count (default-to u0 (get count (map-get? case-type-index {case-type: case-type}))))
         (status-count (default-to u0 (get count (map-get? status-search-index {status: status})))))
        (map-set jurisdiction-cases
            {jurisdiction: jurisdiction, jurisdiction-index: jurisdiction-count}
            {case-id: case-id})
        (map-set jurisdiction-index
            {jurisdiction: jurisdiction}
            {count: (+ jurisdiction-count u1)})
        (map-set case-type-cases
            {case-type: case-type, type-index: type-count}
            {case-id: case-id})
        (map-set case-type-index
            {case-type: case-type}
            {count: (+ type-count u1)})
        (map-set status-cases
            {status: status, status-index: status-count}
            {case-id: case-id})
        (map-set status-search-index
            {status: status}
            {count: (+ status-count u1)})
        (ok true)))

(define-read-only (search-by-jurisdiction (jurisdiction (string-ascii 32)) (limit uint) (offset uint))
    (let
        ((total-count (default-to u0 (get count (map-get? jurisdiction-index {jurisdiction: jurisdiction}))))
         (search-limit (if (> limit max-search-results) max-search-results limit)))
        (asserts! (< offset total-count) err-invalid-search)
        (ok {
            total: total-count,
            limit: search-limit,
            offset: offset
        })))

(define-read-only (get-jurisdiction-case (jurisdiction (string-ascii 32)) (index uint))
    (map-get? jurisdiction-cases {jurisdiction: jurisdiction, jurisdiction-index: index}))

(define-read-only (search-by-case-type (case-type (string-ascii 32)) (limit uint) (offset uint))
    (let
        ((total-count (default-to u0 (get count (map-get? case-type-index {case-type: case-type}))))
         (search-limit (if (> limit max-search-results) max-search-results limit)))
        (asserts! (< offset total-count) err-invalid-search)
        (ok {
            total: total-count,
            limit: search-limit,
            offset: offset
        })))

(define-read-only (get-case-type-case (case-type (string-ascii 32)) (index uint))
    (map-get? case-type-cases {case-type: case-type, type-index: index}))

(define-read-only (search-by-status (status (string-ascii 16)) (limit uint) (offset uint))
    (let
        ((total-count (default-to u0 (get count (map-get? status-search-index {status: status}))))
         (search-limit (if (> limit max-search-results) max-search-results limit)))
        (asserts! (< offset total-count) err-invalid-search)
        (ok {
            total: total-count,
            limit: search-limit,
            offset: offset
        })))

(define-read-only (get-status-case (status (string-ascii 16)) (index uint))
    (map-get? status-cases {status: status, status-index: index}))

(define-read-only (get-jurisdiction-count (jurisdiction (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? jurisdiction-index {jurisdiction: jurisdiction}))))

(define-read-only (get-case-type-count (case-type (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-type-index {case-type: case-type}))))

(define-read-only (get-status-count (status (string-ascii 16)))
    (get count (default-to {count: u0} (map-get? status-search-index {status: status}))))

(define-constant err-subscription-exists (err u111))
(define-constant err-subscription-not-found (err u112))
(define-constant err-notification-not-found (err u113))
(define-constant max-notifications u1000)

(define-data-var notification-counter uint u0)

(define-map case-subscriptions
    { subscriber: principal, case-id: (string-ascii 32) }
    {
        subscription-id: uint,
        alert-types: uint,
        created-timestamp: uint
    }
)

(define-map entity-subscriptions
    { subscriber: principal, subscription-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map subscription-counts
    { subscriber: principal }
    { count: uint }
)

(define-map notifications
    { notification-id: uint }
    {
        recipient: principal,
        case-id: (string-ascii 32),
        notification-type: uint,
        message: (string-ascii 256),
        timestamp: uint,
        is-read: bool
    }
)

(define-map entity-notifications
    { recipient: principal, notification-index: uint }
    { notification-id: uint }
)

(define-map notification-counts
    { recipient: principal }
    { total: uint, unread: uint }
)

(define-constant alert-type-status-change u1)
(define-constant alert-type-document-added u2)
(define-constant alert-type-document-verified u4)
(define-constant alert-type-all u7)

(define-private (has-alert-type (alert-types uint) (alert-type uint))
    (> (bit-and alert-types alert-type) u0))

(define-private (create-notification (recipient principal) (case-id (string-ascii 32)) (notification-type uint) (message (string-ascii 256)))
    (let
        ((notification-id (var-get notification-counter))
         (current-time stacks-block-height)
         (counts (default-to {total: u0, unread: u0} (map-get? notification-counts {recipient: recipient})))
         (total-count (get total counts))
         (unread-count (get unread counts)))
        (asserts! (< notification-id max-notifications) (err u114))
        (map-set notifications
            {notification-id: notification-id}
            {recipient: recipient, case-id: case-id, notification-type: notification-type,
             message: message, timestamp: current-time, is-read: false})
        (map-set entity-notifications
            {recipient: recipient, notification-index: total-count}
            {notification-id: notification-id})
        (map-set notification-counts
            {recipient: recipient}
            {total: (+ total-count u1), unread: (+ unread-count u1)})
        (var-set notification-counter (+ notification-id u1))
        (ok notification-id)))

(define-private (send-case-notifications (case-id (string-ascii 32)) (notification-type uint) (message (string-ascii 256)))
    (ok true))

(define-public (subscribe-to-case (case-id (string-ascii 32)) (alert-types uint))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (subscription-key {subscriber: caller, case-id: case-id})
         (subscription-id (var-get notification-counter))
         (sub-count (default-to u0 (get count (map-get? subscription-counts {subscriber: caller})))))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (> (get-case-count case-id) u0) err-case-not-found)
        (asserts! (is-none (map-get? case-subscriptions subscription-key)) err-subscription-exists)
        (asserts! (<= alert-types alert-type-all) err-invalid-search)
        (map-set case-subscriptions
            subscription-key
            {subscription-id: subscription-id, alert-types: alert-types, created-timestamp: current-time})
        (map-set entity-subscriptions
            {subscriber: caller, subscription-index: sub-count}
            {case-id: case-id})
        (map-set subscription-counts
            {subscriber: caller}
            {count: (+ sub-count u1)})
        (ok subscription-id)))

(define-public (unsubscribe-from-case (case-id (string-ascii 32)))
    (let
        ((caller tx-sender)
         (subscription-key {subscriber: caller, case-id: case-id}))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (is-some (map-get? case-subscriptions subscription-key)) err-subscription-not-found)
        (ok (map-delete case-subscriptions subscription-key))))

(define-public (mark-notification-read (notification-id uint))
    (let
        ((caller tx-sender)
         (notification (unwrap! (map-get? notifications {notification-id: notification-id}) err-notification-not-found))
         (counts (default-to {total: u0, unread: u0} (map-get? notification-counts {recipient: caller}))))
        (asserts! (is-eq caller (get recipient notification)) err-not-authorized)
        (asserts! (not (get is-read notification)) (ok true))
        (map-set notifications
            {notification-id: notification-id}
            (merge notification {is-read: true}))
        (map-set notification-counts
            {recipient: caller}
            {total: (get total counts), unread: (- (get unread counts) u1)})
        (ok true)))

(define-read-only (get-subscription-info (subscriber principal) (case-id (string-ascii 32)))
    (map-get? case-subscriptions {subscriber: subscriber, case-id: case-id}))

(define-read-only (get-entity-subscription (subscriber principal) (index uint))
    (map-get? entity-subscriptions {subscriber: subscriber, subscription-index: index}))

(define-read-only (get-subscription-count (subscriber principal))
    (get count (default-to {count: u0} (map-get? subscription-counts {subscriber: subscriber}))))

(define-read-only (get-notification (notification-id uint))
    (map-get? notifications {notification-id: notification-id}))

(define-read-only (get-entity-notification (recipient principal) (index uint))
    (map-get? entity-notifications {recipient: recipient, notification-index: index}))

(define-read-only (get-notification-counts (recipient principal))
    (default-to {total: u0, unread: u0} (map-get? notification-counts {recipient: recipient})))

(define-read-only (is-subscribed-to-case (subscriber principal) (case-id (string-ascii 32)))
    (is-some (map-get? case-subscriptions {subscriber: subscriber, case-id: case-id})))

(define-constant err-deadline-not-found (err u115))
(define-constant err-deadline-exists (err u116))
(define-constant max-deadlines-per-case u20)

(define-constant deadline-type-filing "filing")
(define-constant deadline-type-hearing "hearing")
(define-constant deadline-type-response "response")
(define-constant deadline-type-appeal "appeal")
(define-constant deadline-type-discovery "discovery")

(define-map case-deadlines
    { case-id: (string-ascii 32), deadline-index: uint }
    {
        deadline-type: (string-ascii 32),
        deadline-block: uint,
        description: (string-ascii 256),
        set-by: principal,
        created-at: uint,
        is-met: bool,
        met-at: (optional uint)
    }
)

(define-map case-deadline-counts
    { case-id: (string-ascii 32) }
    { count: uint }
)

(define-public (set-case-deadline
    (case-id (string-ascii 32))
    (deadline-type (string-ascii 32))
    (deadline-block uint)
    (description (string-ascii 256)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (deadline-count (default-to u0 (get count (map-get? case-deadline-counts {case-id: case-id})))))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (> (get-case-count case-id) u0) err-case-not-found)
        (asserts! (< deadline-count max-deadlines-per-case) err-deadline-exists)
        (asserts! (> deadline-block current-time) err-invalid-search)
        (map-set case-deadlines
            {case-id: case-id, deadline-index: deadline-count}
            {deadline-type: deadline-type, deadline-block: deadline-block,
             description: description, set-by: caller, created-at: current-time,
             is-met: false, met-at: none})
        (map-set case-deadline-counts
            {case-id: case-id}
            {count: (+ deadline-count u1)})
        (ok deadline-count)))

(define-public (mark-deadline-met
    (case-id (string-ascii 32))
    (deadline-index uint))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (deadline-info (unwrap! (map-get? case-deadlines {case-id: case-id, deadline-index: deadline-index}) err-deadline-not-found)))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (not (get is-met deadline-info)) err-already-verified)
        (map-set case-deadlines
            {case-id: case-id, deadline-index: deadline-index}
            (merge deadline-info {is-met: true, met-at: (some current-time)}))
        (ok current-time)))

(define-read-only (get-case-deadline (case-id (string-ascii 32)) (deadline-index uint))
    (map-get? case-deadlines {case-id: case-id, deadline-index: deadline-index}))

(define-read-only (get-case-deadline-count (case-id (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-deadline-counts {case-id: case-id}))))

(define-read-only (is-deadline-expired (case-id (string-ascii 32)) (deadline-index uint))
    (match (map-get? case-deadlines {case-id: case-id, deadline-index: deadline-index})
        deadline-info
            (if (get is-met deadline-info)
                false
                (>= stacks-block-height (get deadline-block deadline-info)))
        false))

(define-read-only (is-deadline-upcoming (case-id (string-ascii 32)) (deadline-index uint) (blocks-threshold uint))
    (match (map-get? case-deadlines {case-id: case-id, deadline-index: deadline-index})
        deadline-info
            (let
                ((blocks-until-deadline (- (get deadline-block deadline-info) stacks-block-height)))
                (if (get is-met deadline-info)
                    false
                    (and (> blocks-until-deadline u0) (<= blocks-until-deadline blocks-threshold))))
        false))

(define-constant err-access-denied (err u117))
(define-constant err-permission-exists (err u118))
(define-constant err-invalid-permission (err u119))

(define-constant permission-view u1)
(define-constant permission-edit u2)
(define-constant permission-manage u4)
(define-constant permission-full u7)

(define-map case-access-control
    { case-id: (string-ascii 32), entity: principal }
    {
        permissions: uint,
        granted-by: principal,
        granted-at: uint,
        expires-at: (optional uint)
    }
)

(define-map case-access-list
    { case-id: (string-ascii 32), access-index: uint }
    { entity: principal }
)

(define-map case-access-counts
    { case-id: (string-ascii 32) }
    { count: uint }
)

(define-map entity-case-access
    { entity: principal, entity-access-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map entity-access-counts
    { entity: principal }
    { count: uint }
)

(define-private (has-permission (permissions uint) (required-permission uint))
    (> (bit-and permissions required-permission) u0))

(define-private (check-case-access (case-id (string-ascii 32)) (entity principal) (required-permission uint))
    (let
        ((access-info (map-get? case-access-control {case-id: case-id, entity: entity}))
         (current-time stacks-block-height))
        (match access-info
            info
                (let
                    ((is-expired (match (get expires-at info)
                                     expiry (>= current-time expiry)
                                     false)))
                    (and (not is-expired) (has-permission (get permissions info) required-permission)))
            false)))

(define-public (grant-case-access
    (case-id (string-ascii 32))
    (entity principal)
    (permissions uint)
    (expires-at (optional uint)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (access-count (default-to u0 (get count (map-get? case-access-counts {case-id: case-id}))))
         (entity-count (default-to u0 (get count (map-get? entity-access-counts {entity: entity})))))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (> (get-case-count case-id) u0) err-case-not-found)
        (asserts! (<= permissions permission-full) err-invalid-permission)
        (asserts! (is-none (map-get? case-access-control {case-id: case-id, entity: entity})) err-permission-exists)
        (match expires-at
            expiry (asserts! (> expiry current-time) err-invalid-search)
            true)
        (map-set case-access-control
            {case-id: case-id, entity: entity}
            {permissions: permissions, granted-by: caller, granted-at: current-time, expires-at: expires-at})
        (map-set case-access-list
            {case-id: case-id, access-index: access-count}
            {entity: entity})
        (map-set case-access-counts
            {case-id: case-id}
            {count: (+ access-count u1)})
        (map-set entity-case-access
            {entity: entity, entity-access-index: entity-count}
            {case-id: case-id})
        (map-set entity-access-counts
            {entity: entity}
            {count: (+ entity-count u1)})
        (ok true)))

(define-public (revoke-case-access
    (case-id (string-ascii 32))
    (entity principal))
    (let
        ((caller tx-sender)
         (access-info (unwrap! (map-get? case-access-control {case-id: case-id, entity: entity}) err-access-denied)))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (or (is-eq caller (get granted-by access-info)) (is-eq caller contract-owner)) err-not-authorized)
        (ok (map-delete case-access-control {case-id: case-id, entity: entity}))))

(define-public (update-case-permissions
    (case-id (string-ascii 32))
    (entity principal)
    (new-permissions uint))
    (let
        ((caller tx-sender)
         (access-info (unwrap! (map-get? case-access-control {case-id: case-id, entity: entity}) err-access-denied)))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (<= new-permissions permission-full) err-invalid-permission)
        (asserts! (or (is-eq caller (get granted-by access-info)) (is-eq caller contract-owner)) err-not-authorized)
        (map-set case-access-control
            {case-id: case-id, entity: entity}
            (merge access-info {permissions: new-permissions}))
        (ok true)))

(define-read-only (get-case-access (case-id (string-ascii 32)) (entity principal))
    (map-get? case-access-control {case-id: case-id, entity: entity}))

(define-read-only (has-case-access (case-id (string-ascii 32)) (entity principal) (required-permission uint))
    (check-case-access case-id entity required-permission))

(define-read-only (get-case-access-list-entry (case-id (string-ascii 32)) (access-index uint))
    (map-get? case-access-list {case-id: case-id, access-index: access-index}))

(define-read-only (get-case-access-count (case-id (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-access-counts {case-id: case-id}))))

(define-read-only (get-entity-case-access (entity principal) (entity-access-index uint))
    (map-get? entity-case-access {entity: entity, entity-access-index: entity-access-index}))

(define-read-only (get-entity-access-count (entity principal))
    (get count (default-to {count: u0} (map-get? entity-access-counts {entity: entity}))))

(define-read-only (is-access-expired (case-id (string-ascii 32)) (entity principal))
    (match (map-get? case-access-control {case-id: case-id, entity: entity})
        access-info
            (match (get expires-at access-info)
                expiry (>= stacks-block-height expiry)
                false)
        true))
