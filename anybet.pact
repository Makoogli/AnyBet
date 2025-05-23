(namespace "free")
(module kadena-place-anybet GOV
    (defcap GOV
        "module governance"
        (enforce-keyset "free.kadena-place-admin")
    )

    ; Schemas

    (defschema user-schema
        previous-user:string
        last-hosted:string
        last-placed:string
        bets-hosted:integer
        bets-placed:integer
        likes-received:integer
        dislikes-received:integer
        favorite-users:[string]
        kda-won:decimal
        kda-lost:decimal
        kda-provided:decimal
        kda-hosted:decimal
        date-joined:time
    )

    (defschema hosted-bet-schema
        previous-host-bet:string
        previous-hosted-bet:string
        previous-tag-bets:list
        tags:list
        name:string
        host:string
        description:string
        host-fee:decimal
        left:{option}
        right:{option}
        date-opens:time
        date-closes:time
        likes:integer
        dislikes:integer
        status:string ; pending,left,right,cancelled
    )

    (defschema option-schema
        name:string
        description:string
        color:string
        kda-provided:decimal
        kda-free:decimal
        kda-locked:decimal
    )

    (defschema placed-bet-schema
        previous-user-bet:string
        previous-placed-bet:string
        previous-tag-bets:list
        hosted-bet:string
        left:bool
        kda-placed:decimal
        kda-locked:decimal
    )

    (defschema last-schema
        id:string
        ; hosted-bet:string ; recently created bets
        ; placed-bet:string ; recently placed bets
        ; new-user:string ; users recently joined
        ; new-tag:string ; tags recently created
    )

    (defschema tag-schema ; id is name of tag
        previous-tag:string
        last-hosted-bet:string
        last-placed-bet:string
        bets-hosted:integer
        bets-placed:integer
    )

    (defschema time-line-years-schema
        months:list
    )

    (defschema time-line-months-schema
        days:list
    )

    (defschema time-line-days-schema
        hours:list
    )

    (defschema time-line-hours-schema
        minutes:list
    )

    (defschema time-line-minutes-schema
        bets:list
    )

    ; Tables

    (deftable user-table:{user-schema})

    (deftable hosted-bet-table:{hosted-bet-schema})

    (deftable placed-bet-table:{placed-bet-table})

    (deftable last-table:{last-schema})

    (deftable tag-table:{tag-schema})

    (deftable time-line-years-table:{time-line-years-schema})

    (deftable time-line-months-table:{time-line-months-schema})

    (deftable time-line-days-table:{time-line-days-schema})

    (deftable time-line-hours-table:{time-line-hours-schema})

    (deftable time-line-minutes-table:{time-line-minutes-schema})

    ; Constants

    (defconst kda-pool:string "anybet-pool")

    ; Functions

    (defun initialize ()
        (coin.create-account kda-pool (create-user-guard (enforce-pool-guard)))
        (insert last-table "hosted-bet" {"id":""})
        (insert last-table "placed-bet" {"id":""})
        (insert last-table "new-user" {"id":""})
        (insert last-table "new-tag" {"id":""})
        (insert last-table "year" {"id":"2025"})
        (insert time-line-years-table "2025" {"months":[]})
    )

    (defun new-user ()
        (insert user-table (at "sender" (chain-data))
            {
                "previous-user":(at "id" (read last-table "new-user")),
                "last-hosted":"",
                "last-placed":"",
                "bets-hosted":0,
                "bets-placed":0,
                "likes-received":0,
                "dislikes-received":0,
                "favorite-users":[],
                "kda-won":0.0,
                "kda-lost":0.0,
                "kda-provided":0.0,
                "kda-hosted":0.0,
                "date-joined":(at "block-time" (chain-data))
            }
        )
        (update last-table "new-user" {"id":(at "sender" (chain-data))})
    )

    (defun new-tag (name:string)
        (insert tag-table name {"previous-tag":(at "id" (read last-table "new-tag")),"last-hosted-bet":"","last-placed-bet":"","bets-hosted":0,"bets-placed":0})
        (update last-table "new-tag" {"id":name})
    )

    ; ################### put time-line in host-bet

    (defun host-bet (tags:list name:string description:string host-fee:decimal left-name:string left-description:string left-color:string left-provided:decimal right-name:string right-description:string right-color:string right-provided:decimal date-opens:time date-closes:time)
        (let*
            (
                (host-id:string (at "sender" (chain-data)))
                (host-data:{user-schema} (read user-table host-id))
                (bet-id:string (hash (at "id" (read last-table "hosted-bet"))))
            )
            (enforce (<= 0 host-fee) "host-fee can't be negative")
            (enforce (<= host-fee (max-host-fee (at "bets-hosted" host-data))) "host-fee is too high")
            (validate-color left-color)
            (validate-color right-color)
            (enforce (< 0 left-provided) "left-provided has to be positive")
            (enforce (< 0 right-provided) "right-provided has to be positive")
            (enforce (and (< date-opens date-closes) (< (at "block-time" (chain-data)) date-closes)) "can't close the bet before it opens")
            (with-read last-table "hosted-bet" {"id":=previous-hosted-bet-id}
                (insert hosted-bet-table bet-id
                    {
                        "previous-host-bet":(at "last-hosted" host-data),
                        "previous-hosted-bet":previous-hosted-bet-id,
                        "previous-tag-bets":(map
                            (lambda
                                (tag:string)
                                (with-read tag-table tag {"last-hosted-bet":=last-hosted-bet-id,"bets-hosted":=bets-hosted}
                                    (update tag-table tag {"last-hosted-bet":bet-id,"bets-hosted":(+ 1 bets-hosted)})
                                    last-hosted-bet-id
                                )
                            )
                            tags
                        ),
                        "tags":tags,
                        "name":name,
                        "host":host-id,
                        "description":description,
                        "host-fee":host-fee,
                        "left":{"name":left-name,"description":left-description,"color":left-color,"kda-provided":left-provided,"kda-free":left-provided,"kda-locked":0.0},
                        "right":{"name":right-name,"description":right-description,"color":right-color,"kda-provided":right-provided,"kda-free":right-provided,"kda-locked":0.0},
                        "date-opens":date-opens,
                        "date-closes":date-closes,
                        "likes":0,
                        "dislikes":0,
                        "status":"pending"
                    }
                )
            )
            (update last-table "hosted-bet" {"id":bet-id})
            (update user-table host-id {"last-hosted":bet-id,"bets-hosted":(+ 1 (at "bets-hosted" host-data)),"kda-provided":(+ (+ left-provided right-provided) (at "kda-provided" host-data))})
            (coin.transfer host-id kda-pool (* (+ left-provided right-provided) (+ 1 (/ (+ 1 (length tags)) 100.0))))
        )
    )

    (defun call-bet (hosted-bet-id:string left-wins:bool)
        (let*
            (
                (hosted-bet-data:{hosted-bet-schema} (read hosted-bet-table hosted-bet-id))
                (host-id:string (at "sender" (chain-data)))
                (amount:decimal (+ (+ (at "kda-provided" (at "left" hosted-bet-data)) (at "kda-provided" (at "right" hosted-bet-data))) (* 0.95 (at "kda-free" (at (if left-wins "right" "left") hosted-bet-data)))))
            )
            (enforce (= host-id (at "host" hosted-bet-data)) "you dont have permission to call this bet")
            (enforce (= "pending" (at "status" hosted-bet-data)) "bet has already been called or cancelled")
            (update hosted-bet-table hosted-bet-id {"status":(if left-wins "left" "right")})
            (with-capability (POOL-GUARD)
                (install-capability (coin.TRANSFER kda-pool host-id amount))
                (coin.transfer kda-pool host-id amount)
            )
        )
    )

    (defun cancel-bet (hosted-bet-id:string)
        (let*
            (
                (hosted-bet-data:{hosted-bet-schema} (read hosted-bet-table hosted-bet-id))
                (host-id:string (at "sender" (chain-data)))
                (amount:decimal (* 0.95 (+ (at "kda-provided" (at "left" hosted-bet-data)) (at "kda-provided" (at "right" hosted-bet-data)))))
            )
            (enforce (= host-id (at "host" hosted-bet-data)) "you dont have permission to call this bet")
            (enforce (= "pending" (at "status" hosted-bet-data)) "bet has already been called or cancelled")
            (update hosted-bet-table hosted-bet-id {"status":"cancelled"})
            (with-capability (POOL-GUARD)
                (install-capability (coin.TRANSFER kda-pool host-id amount))
                (coin.transfer kda-pool host-id amount)
            )
        )
    )

    (defun place-bet)

    (defun claim-winnings)

    (defun review-bet)

    ; Local Functions

    (defun get-user-data:{user-schema} (user-id:string)
        (read user-table user-id)
    )

    (defun get-host-bets:[{hosted-bet-schema}] (hosted-bet-id:string max:integer)
        (enforce (<= 1 max) "max has to be atleast 1")
        (let*
            (
                (iter:list
                    (enumerate 1 max)
                )
            )
            (reverse
                (take
                    max
                    (fold
                        (lambda
                            (
                                acc:list
                                i:integer
                            )
                            (+ [(read hosted-bet-table (at "previous-host-bet" (at 0 acc)))] acc)
                        )
                        [{"previous-host-bet":hosted-bet-id}]
                        iter
                    )
                )
            )
        )
    )

    (defun get-user-bets:[{placed-bet-schema}] (placed-bet-id:string max:integer)
        (enforce (<= 1 max) "max has to be atleast 1")
        (let*
            (
                (iter:list
                    (enumerate 1 max)
                )
            )
            (reverse
                (take
                    max
                    (fold
                        (lambda
                            (
                                acc:list
                                i:integer
                            )
                            (+ [(read placed-bet-table (at "previous-user-bet" (at 0 acc)))] acc)
                        )
                        [{"previous-user-bet":placed-bet-id}]
                        iter
                    )
                )
            )
        )
    )

    ; Helper Functions

    (defun max-host-fee:decimal (bets-hosted:integer)
        (round (/ (ln (+ bets-hosted 1)) 100) 8)
    )

    (defun time-line-minutes-id:integer (time:time)
        (format-time "%y-%m-%d-%H-%M" time)
    )

    (defun time-line-hours-id:integer (time:time)
        (format-time "%y-%m-%d-%H" time)
    )

    (defun time-line-days-id:integer (time:time)
        (format-time "%y-%m-%d" time)
    )

    (defun time-line-months-id:integer (time:time)
        (format-time "%y-%m" time)
    )

    (defun time-line-years-id:string (time:time)
        (format-time "%y" time)
    )

    (defun validate-color (color:string)
        (enforce (= (length color) 7) "Invalid color length")
        (enforce (= (at 0 (str-to-list color)) "#") "Color must start with #")
        (enforce (= (length (filter (= "#") (str-to-list color))) 1) "Invalid color too many #s")
        (enforce (fold (and) true (map (lambda (x) (contains x ["#","0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"])) (str-to-list color))) "Invalid color characters")
    )
)