; (module zoo-game LOTTERY-GOVERNANCE

(define-keyset 'zookeeper (read-keyset "zookeeper"))

(module zoo-game 'zookeeper


  ;use coin contract for payments
  (use coin)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables
  ; ---------------------------------------------------------------------

  (defschema user-history
    @doc "Table to record the behavior user history of games, \
    \  updated each time a player makes a bet"
    placed-bets:list
    coins-bet:decimal
    coins-won:decimal
  )

  (defschema games-history
    @doc "store overall info about all games \
    \ updated  the game is played"
    total-coins-in:decimal
    total-coins-out:decimal
    total-bets:integer
    total-games-played:integer
  )

  (defschema games
    @doc "Table to record the bets in each game, \
    \ id is going to be the projected timestamp of the game \
    \  table will be updated each time a bet is submitted"
    draw:list
    ;verify whether time is a string or its own type
    draw-time:string
    status:string
    coins-in:decimal
    coins-out:decimal
    total-bets:integer
    animal-bets:list
    numbers-bets:list
    odd-multiplier:decimal
  )

  (deftable user-history-table:{user-history})

  (deftable games-history-table:{games-history})

  (deftable games-table:{games})

  ; --------------------------------------------------------------------------
  ; Constants and Capabilities
  ; --------------------------------------------------------------------------

  (defconst LOTTERY_ACCOUNT:string 'sender00)

  (defconst GAME_OPEN:string "open")
  (defconst GAME_CLOSED:string "closed")


  (defconst NUMBERS_PAYOUTS:list [[50 7] [500 60] [5000 600]])
  (defconst ANIMAL_PAYOUTS:list [[12.0 [3.0]]
                                  [95.0 [1.0 12.0]]
                                    [700.0 [0.75 3.0 42.0]]
                                      [4000.0 [0.2 1.5 22.0 500.0]]
                                        [17000.0 [0.2 1.0 8.0 150.0]]])

  (defun lottery-guard:guard () (create-module-guard 'lottery-admin))

  (defcap ADMIN ()
    "makes sure only contract owner can make important function calls"
    (enforce-keyset 'zookeeper)
  )


  ; --------------------------------------------------------------------------
  ; ZOO GAME CONTRACT -- main functions
  ; --------------------------------------------------------------------------


  (defun init-games-history-table ()
    @doc "called at contract genesis \
    \ note: table will only have this overall row"
    (with-capability (ADMIN)
        (insert games-history-table "overall" {
          "total-coins-in": 0.0,
          "total-coins-out": 0.0,
          "total-bets": 0,
          "total-games-played": 0 })
    )
  )


  (defun init-game (game-id:string multiplier:decimal)
    @doc "allows admin to start a new game"
    (with-capability (ADMIN)
    ;game-id is probably gonna be projected draw date..
        (insert games-table game-id {
          "draw": [],
          "draw-time": "",
          "status": GAME_OPEN,
          "coins-in": 0.0,
          "coins-out": 0.0,
          "total-bets": 0,
          "animal-bets":[],
          "numbers-bets": [],
          "odd-multiplier": multiplier})
    )
  )

  (defun end-game (game-id:string)
    @doc "allows admin to end game and pay winning bets"
    (with-capability (ADMIN)
        (update games-table game-id {
          "draw": (make-draw),
          ;missing time of when tx actually goes through...
          "status": GAME_CLOSED})
        ;make sure this gets executed after...
        (pay-game game-id)
    )
  )


  ; --------------------------------------------------------------------------
  ; Random functions
  ; --------------------------------------------------------------------------


  (defun make-draw:list ()
    @doc "reads from (chain-data) to take in block-time, \
    \ block-height, prev-block-hash, also reads total-bets (not for now)\
    \   from games table and will potentially take one more input from FE \
    \ hashes it, then takes last four digits of str-to-int base 64 to create a list of 5 outcomes"
    (require-capability (ADMIN))
    (let* ((chain-seeds (chain-data))
          (block-time-hash (hash (at "block-time" chain-seeds)))
          (prev-block-hash (hash (at "prev-block-hash" chain-seeds)))
          (block-height-hash (hash (at "block-height"chain-seeds)))
          (master-hash-int (str-to-int 64 (hash (+ block-height-hash (+ prev-block-hash block-time-hash)))))
          (master-hash-str (format "{}" [master-hash-int]))
          (pos-one (take -4 master-hash-str))
          (master-hash-str (drop -4 master-hash-str))
          (pos-two (take -4 master-hash-str))
          (master-hash-str (drop -4 master-hash-str))
          (pos-three (take -4 master-hash-str))
          (master-hash-str (drop -4 master-hash-str))
          (pos-four (take -4 master-hash-str))
          (master-hash-str (drop -4 master-hash-str))
          (pos-five (take -4 master-hash-str))
        )
        ;note that the draw ranking is in reverse order
        [pos-five pos-four pos-three pos-two pos-one]
    )
  )


  ; --------------------------------------------------------------------------
  ; Betting functions -> for players
  ; --------------------------------------------------------------------------


  (defun bet-number (bet:string game-id:string account:string amount:decimal)
    @doc "allows a user registered in this contract to place number a bet \
    \ with format  "01" -> tens; "102" -> hundreds; "1002" -> thousands"

    (enforce (and (>= (length bet) 2) (and (>= (str-to-int bet) 0) (<= (str-to-int bet) 9999)))
        "wrong format for numbers bet. Please enter a string with two to four digits")
    ;takes care of enforcing selected game exists and is open
    (with-read games-table game-id
      {"numbers-bets":=numbers-list,
       "total-bets":=total-bets,
       "coins-in":=coins-in,
       "status":= status}

    ;make sure game is still open
        ;UNSURE: behaviour when bet and end-game are in the same tx...
    (enforce (= status GAME_OPEN) "you are too late to bet on selected game")

    ;user sends in money for the bet
        ;takes care of failure if user isnt registered in coin or not enough funds
    (transfer account LOTTERY_ACCOUNT amount)

    ;using default-read in case it is a new user
    (with-default-read user-history-table account
      {"placed-bets": [],
      "coins-bet": 0.0,
      "coins-won": 0.0}
      { "placed-bets":= past-bets-list,
        "coins-bet":= coins-bet,
        "coins-won":= coins-won}

      (update games-table game-id
        { "numbers-bets": (+ numbers-list [[bet, amount, account]]),
          "total-bets": (+ total-bets 1),
          "coins-in": (+ coins-in amount)})
      ;write in case user is first time better
      (write user-history-table account
        { "placed-bets": (+ past-bets-list [[bet, amount, game-id]]),
          "coins-bet": (+ coins-bet amount),
          "coins-won": coins-won})
    )
   )
  )


  (defun bet-animal (bet:list game-id:string account:string amount:decimal)
    @doc "allows a user registered in this contract to place an animal bet \
    \ with format [["00" "01" "02" "03"] ... ["64" "65" "66" "67"]] of max len 5"

    (enforce (<= (length bet) 5)
          "wrong betting format for animal")
    (enforce (= (map (length) bet) (make-list (length bet) 4))
            "wrong betting format for animal")
    ;takes care of enforcing selected game exists and is open
    (with-read games-table game-id
        {"animal-bets":=animal-list,
        "total-bets":=total-bets,
        "coins-in":=coins-in,
        "status":= status}
      ;make sure game is still open
        ;UNSURE: behaviour when bet and end-game are in the same tx...
      (enforce (= status GAME_OPEN) "you are too late to bet on selected game")

      ;user sends in money for the bet
        ;takes care of failure if user isnt registered in coin or not enough funds
      (transfer account LOTTERY_ACCOUNT amount)

      ;using default-read in case it is a new user
      (with-default-read user-history-table account
        {"placed-bets": [],
        "coins-bet": 0.0,
        "coins-won": 0.0}
        { "placed-bets":= past-bets-list,
          "coins-bet":= coins-bet,
          "coins-won":= coins-won}

          (update games-table game-id
            { "animal-bets": (+ animal-list [[bet, amount, account]]),
              "total-bets": (+ total-bets 1),
              "coins-in": (+ coins-in amount)})
          ;write in case user is first time better
          (write user-history-table account
            { "placed-bets": (+ past-bets-list [[bet, amount, game-id]]),
              "coins-bet": (+ coins-bet amount),
              "coins-won": coins-won})
      )
    )
  )

  ; --------------------------------------------------------------------------
  ; Pay functions -> for admin
  ; --------------------------------------------------------------------------


  (defun pay-winner (game-id:string winner-account:string amount-out:decimal)
    @doc "ADMIN ONLY: pays winner through coin contract and updates tables"
    (require-capability (ADMIN))
    (transfer LOTTERY_ACCOUNT winner-account amount-out)
    (with-read user-history-table winner-account
      {"coins-won":=coins-won}
      (update user-history-table winner-account
        {"coins-won": (+ amount-out coins-won)})
    )
    (with-read games-table game-id
      {"coins-out":= coins-out}
      (update games-table game-id
        {"coins-out": (+ coins-out amount-out)})
    )
  )


  (defun find-pay-number-winners (game-id:string draw:list odd-mult:decimal bet:list)
    @doc "ADMIN ONLY: pays big odd if number is in first position of draw \
    \ pays small odd if number is any other position in draw"
    (require-capability (ADMIN))
    (let* ((bet-length (length (at 0 bet)))
          (odd-index (- bet-length 2))
          (draw-mod (map (take (* -1 bet-length)) draw)))
    ;pay small odd if number bet is in not 1st position
    (if (contains (at 0 bet) (take 4 draw-mod)) (pay-winner game-id (at 2 bet) (* (* (at 0 (at odd-index NUMBERS_PAYOUTS)) (at 1 bet)) odd-mult)) "didn't win number")
    ;pay big odd if number bet is in 1st position (last in array)
    (if (contains (at 0 bet) (take -1 draw-mod)) (pay-winner game-id (at 2 bet) (* (* (at 1 (at odd-index NUMBERS_PAYOUTS)) (at 1 bet)) odd-mult)) "didn't win number")
    ;figure out how to give 1x back for 'right animal'
    )
  )


  (defun pay-number-bets (game-id:string)
    @doc "ADMIN ONLY: initiates number bets payouts"
    (require-capability (ADMIN))
    (with-read games-table game-id {
      "numbers-bets":= numbers-bets,
      "draw":= draw,
      "odd-multiplier":= odd-mult}
        (map (find-pay-number-winners game-id draw odd-mult) numbers-bets)
    )
  )

  (defun map-animal-helper (bet:list animal-two-digit:string)
    (map (contains animal-two-digit) bet)
  )

  (defun filter-animal-helper (bet:list animal-two-digit:string)
    (let ((len-elem (length (filter (= true) (map (contains animal-two-digit) bet)))))
        (if (>= len-elem 1) 1 0)
    )
  )

  (defun find-pay-animal-other (draw-mod:list odds:list game-id:string bet:list)
    @doc "ADMIN ONLY: checks for animal bets showing up in the draw \
    \ pays out according to how many bets are in the draw"
    (require-capability (ADMIN))
    (let* ((small-win-count (fold (+) 0 (map (filter-animal-helper (at 0 bet)) draw-mod)))
         (max-index (- (length odds) 1))
         (odd-index (if (<= small-win-count max-index) (- small-win-count 1) max-index)))
        ; [odd-index
        ; (map (filter-animal-helper (at 0 bet)) draw-mod)]
        (if (!= small-win-count 0) (pay-winner game-id (at 2 bet) (* (at odd-index odds) (at 1 bet))) "didn't win animal")
    )
  )

  (defun find-pay-animal-big (mod-draw:list game-id:string odd-mult:decimal bet:list)
    @doc "ADMIN ONLY: first checks if animal bet wins 'big' prize (animal(s) in first n positions) \
    \ then checks if animal in other positions"
    (require-capability (ADMIN))
    (let* ((pos-to-check (take (* (length (at 0 bet)) -1) mod-draw))
         (res-list (map (map-animal-helper (at 0 bet)) pos-to-check))
         (hit-count-list (map (filter (= true)) res-list))
         (hit-count (fold (+) 0 (map (length) hit-count-list)))
         (odd-index (- (length (at 0 bet)) 1)))
            (if
              (= hit-count (length pos-to-check))
                    ; (pay-winner game-id (at 2 bet) (* odd-big (at 1 bet)))
                    (pay-winner game-id (at 2 bet) (* (* (at 0 (at odd-index ANIMAL_PAYOUTS)) (at 1 bet)) odd-mult))
                    ;check if won any other animal bet
                      (find-pay-animal-other mod-draw (* (at 1 (at odd-index ANIMAL_PAYOUTS)) odd-mult) game-id bet))
                    ;CORRECT CODE FOR LINE ABOVE
                    ;(find-pay-animal-other mod-draw (map (* odd-mult) (at 1 (at odd-index ANIMAL_PAYOUTS))) game-id bet))
    )
  )

  (defun pay-animal-bets (game-id:string)
    @doc "ADMIN ONLY: initiaties animal bet paying "
    (require-capability (ADMIN))
    (with-read games-table game-id {
      "animal-bets":= animal-bets,
      "draw":= draw,
      "odd-multiplier":= odd-mult
      }
      (map (find-pay-animal-big (map (take -2) draw) game-id odd-mult) animal-bets)
    )
  )

  (defun pay-game (game-id:string)
    @doc "ADMIN ONLY: does all logic to pay players in one function \
    \ updates games-history-table pay calls"
    (require-capability (ADMIN))
    (pay-number-bets game-id)
    (pay-animal-bets game-id)
    (with-read games-table game-id {
      "coins-in":= coins-in-game,
      "coins-out":= coins-out-game,
      "total-bets":= total-bets-game}
      (with-read games-history-table "overall" {
        "total-coins-in":= total-coins-in,
        "total-coins-out":= total-coins-out,
        "total-bets":= total-bets,
        "total-games-played":= played}
       (update games-history-table "overall" {
         "total-coins-in": (+ total-coins-in coins-in-game),
         "total-coins-out": (+ total-coins-out coins-out-game),
         "total-bets": (+ total-bets total-bets-game),
         "total-games-played": (+ played 1)
        })
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; UTILS -> for fetching data ( /local calls )
  ; --------------------------------------------------------------------------


  (defun get-games-table (game-id:string)
    (read games-table game-id)
  )

  (defun get-player (player-id:string)
    (read user-history-table player-id)
  )

  (defun get-history ()
    (read games-history-table "overall")
  )


)

(create-table games-table)
(create-table games-history-table)
(create-table user-history-table)
(init-games-history-table)
