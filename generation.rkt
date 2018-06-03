#lang racket

(require racket/random graph
         "encounters.rkt"
         "dungeon.rkt"
         "state.rkt"
         "grid.rkt"
         "cell.rkt"
         "items.rkt"
         "utils.rkt"
         "message-queue.rkt")

(provide generate)

(define (room-centroid r)
  (match-define (room (vector x y) height width _1 _2) r)
  (vector (+ x (/ height 2.0)) (+ y (/ width 2.0))))

(define (random-room-poss room n)
  (random-sample (room-free-cells room) n #:replacement? #f))
(define (random-room-pos room)
  (first (random-room-poss room 1)))


(define (generate player)
  (define lvl (get-field level player))
  (define-values (theme pre-encounters) (generate-encounters lvl))
  (define encounters (map instantiate-encounter pre-encounters))

  (define-values (backstory boss treasure)
    (generate-backstory theme))
  (enqueue-briefing! "\n\n")
  (enqueue-briefing! backstory)
  (enqueue-briefing! "\n\nGodspeed, and don't break it.\n")

  ;; find all the monsters used
  (define monster-kinds
    (sort
     (set->list (for*/set ([e (in-list encounters)]
                           [m (in-list e)])
                  (cons (send m show) (send m describe))))
     char<? #:key car))
  (enqueue-briefing! "\n") (enqueue-briefing! "\n")
  (enqueue-briefing! "Enemies:")
  (for ([(c d) (in-dict monster-kinds)])
    (enqueue-briefing! (format "~a : ~a" c d)))

  ;; need enough rooms for all the encounters, plus a starting room
  ;; (we don't want monsters in the starting room)
  (define n-encounters (length encounters))
  (match-define (dungeon grid rooms connections)
    (generate-dungeon (add1 n-encounters)))

  ;; build room graph
  (define graph
    (weighted-graph/undirected
     (for*/list ([(r1 r2) (in-dict connections)]
                 ;; `connections` includes corridors, which we don't care
                 ;; about (and who don't keep track of pos, height and width
                 ;; so we can't use them for the graph anyway)
                 ;; Note: we may get nicer results if we could, though
                 #:when (and (room? r1) (room? r2)))
       (define weight
         (manhattan-distance (room-centroid r1) (room-centroid r2)))
       (list weight r1 r2))))

  ;; place player and goal in furthest rooms
  ;; see: roguebasin.com/index.php?title=Creating_Measurably_%22Fun%22_Maps
  ;;  (idea based roughly on that. not following specific technique)
  (define all-pairs-shortest-path (floyd-warshall graph))
  (define-values (player-room goal-room _)
    (for/fold ([player-room  #f]
               [goal-room    #f]
               [max-distance 0])
        ([(r1+r2 distance) (in-hash all-pairs-shortest-path)])
      (match-define (list r1 r2) r1+r2)
      (if (> distance max-distance) ; found a more distant pair
          (values r1 r2 distance)
          (values player-room goal-room max-distance))))
  (define player-pos (random-room-pos player-room))
  (claim-room-cell! player-room player-pos)
  (array-set! grid player-pos (new entrance%))
  (define goal-pos (random-room-pos goal-room))
  (claim-room-cell! goal-room goal-pos)
  (array-set! grid goal-pos (new chest% [items (list treasure)]))

  ;; add decoy items in some (with some probability) of the other rooms
  (for ([r (in-list rooms)]
        #:unless (eq? r goal-room)
        #:when (random-bool bogus-chest-probability))
    (define chest-pos (random-room-pos r))
    (define decoy     (new decoy% [name (generate-bogus-item boss)]))
    (array-set! grid chest-pos (new chest% [items (list decoy)])))

  ;; place encounters
  (define encounter-rooms ; excludes player's room. don't start with monsters
    (random-sample (remove player-room rooms) n-encounters #:replacement? #f))
  (define monster-poss
    (for/list ([e    (in-list encounters)]
               [r    (in-list encounter-rooms)]
               #:when #t ; nest iteration
               [poss (in-value (random-room-poss r (length e)))]
               #:when #t ; nest iteration
               [m    (in-list e)]
               [pos  (in-list poss)])
      (claim-room-cell! r pos)
      (cons m pos)))
  (new-state player grid
             #:characters (cons (cons player player-pos) monster-poss)))

;; Note: can't have newlines in the result, or will confuse line breaking.
;; TODO probably better to fix the line breaking function to deal with newlines
(define (generate-backstory theme)
  (match-define (list title potential-pronouns)
    (random-ref '(("grand poobah" ("his"))
                  ("lord" ("his"))
                  ("lady" ("her"))
                  ("shaman" ("his" "her"))
                  ("high priest" ("his"))
                  ("high priestess" ("her"))
                  ("commander-in-chief" ("his"))
                  ("boss" ("his" "her"))
                  ("chieftain" ("his" "her"))
                  ("grand vizier" ("his"))
                  ("chairman" ("his"))
                  ("chairwoman" ("her"))
                  ("prefect" ("his"))
                  ("general secretary" ("his" "her"))
                  ("warlord" ("his"))
                  ("warlady" ("her"))
                  ("master of ceremonies" ("his"))
                  ("mistress of ceremonies" ("her")))))
  (define pronoun (random-ref potential-pronouns))
  (match-define (list-rest treasure-name
                           treasure-article
                           (or (list treasure-describe-article)
                               (and treasure-describe-article #f)))
    ;; two articles here, one for the backstory (where possessives make sense
    ;; and are nice), and one for in-game description (where they don't)
    (random-ref `(("magical macguffin" "a")
                  ("Golden Goat" "The")
                  ("amulet of bling-bling" ,pronoun "an")
                  (,(string-append "collection of designer "
                                   (random-ref
                                    '("cloaks" "robes" "pantaloons"
                                      "helmets" "vambraces" "greaves")))
                   ,pronoun "a")
                  ("jeweled chandelier" ,pronoun "a")
                  ("convertible sports cart" ,pronoun "a")
                  ("orb of HBO viewing" ,pronoun "an")
                  (,(string-append (random-ref '("amulet" "ring" "earring"))
                                   " of "
                                   (random-ref ominous-names))
                   "the"))))
  (define treasure
    (new macguffin%
         [name    treasure-name]
         [article (or treasure-describe-article treasure-article)]))
  (values
   (string-append
    "You, O adventurer, have been asked to investigate the "
    (random-ref
     (case theme
       [(vermin) '("lair" "den" "burrow" "sty" "dump")]
       [(tomb)   '("tomb" "crypt" "mausoleum" "catacombs")]
       [(castle) '("castle" "tower" "bastion" "fortress" "hideout")]
       [(jungle) '("ruins" "ancient city" "forgotten temple")]))
    " of "
    (random-ref ominous-names)
    ". Its " title " has not been paying " pronoun " "
    (random-ref '("gambling debts" "stronghold-building loan"
                  "potion speculation debts" "student loans"
                  "alimony" "bar tab" "protection money"))
    ", and you must therefore repossess "
    treasure-article " " treasure-name
    " to satisfy " pronoun " creditors.")
   title
   treasure))

(define ominous-names
  '("doom" "tears" "darkness" "evil" "death" "sin" "danger" "peril"
    "mild disappointment" "fear" "unease" "anger" "rage" "fury"
    "spikes" "champions" "foulness"
    "Gargakkhan" "Xyrthyrthilixth" "Barney"))

;; not macguffins. "decoys" of sorts
(define (generate-bogus-item owner)
  (random-ref
   ;; all of those should work with "a" (i.e., no "an")
   ;; because I'm lazy
   `(,(string-append "painting of the " owner "'s "
                     ;; father is not funny enough
                     (random-ref
                      '("mother" "uncle" "aunt" "grand-mother"
                        "great-uncle" "great-aunt"
                        "poodle" "late guinea pig")))
     "dwarven oar"
     "used wig"
     "pile of empty potion bottles"
     "collection of nail clippings"
     "half-eaten turkey"
     "broken vase"
     "dented helmet"
     "stolen sign for The Unarmed Chicken pub"
     "copper saucepan"
     "very large feather"
     ,(string-append "fan letter to the " owner)
     "misplaced guinea pig"
     "stash of instant noodles"
     "vial of plate mail wax"
     "stack of unpaid bills"
     "two-year-old jury summons"
     "miniature shield"
     "bag with a hole in it"
     "single marble"
     )))
;; chance that a room has a chest with a bogus item
(define bogus-chest-probability 0.7)

(module+ main
  (require "grid.rkt" "player.rkt")
  (display (show-grid (state-grid (generate (new player%))))))
