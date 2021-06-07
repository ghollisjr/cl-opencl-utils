(require 'cl-opencl-utils)
(require 'cl-ana)
(defpackage #:yahtzee
  (:use :cl
        :cl-opencl
        :cl-opencl-utils))
(cl-ana.package-utils:use-package-group :cl-ana :yahtzee)
(in-package :yahtzee)

;;;; GPU-accelerated Monte Carlo simulation for the dice-throwing game
;;;; Yahtzee.
;;;;
;;;; One thing to note is the use of #pragma in yahtzee-hist.  At the
;;;; moment there isn't a mechanism for automatic pragma management,
;;;; so the way to implement this is to append pragma statements to
;;;; the beginning of the source code supplied to
;;;; cl-create-program-with-source.
;;;;
;;;; The key demonstration functions are
;;;;
;;;; * #'yahtzee to generate scores
;;;;
;;;; * #'yahtzee-hist to generate histograms of scores (index is
;;;;   score, value is number of games with that score)
;;;;
;;;; * #'draw-yahtzee-hist to time the simulation and draw the results
;;;;   using #'cl-ana.plotting:draw
;;;;
;;;; On my run-of-the-mill GPU, 10 billion games takes ~95 seconds
;;;; round trip (i.e. OpenCL compilation and data transfer included).

(defparameter *maxscore*
  (+ 35
     (* (loop for i from 1 to 6 summing i) 5)
     30
     30
     25
     30
     40
     30
     1250))
(defclcglobalvar
    (var YAHTZEE_MAX_SCORE :ushort 1575))

(defclcstruct scoresheet
  (:top (:array :uchar 6))
  (:tk :uchar) ; 3 of kind
  (:fk :uchar) ; 4 of kind
  (:fh :uchar)  ; full house (0 or 25)
  (:ss :uchar)  ; small straight (0 or 30)
  (:ls :uchar)  ; large straight (0 or 40)
  (:ch :uchar)  ; chance score
  (:y :uchar))  ; number of yahtzees scored

;; value to denote scoresheet slot that hasn't been written yet
(defclcglobalvar
    (var NULLSCORE :uchar 255))

(defclcfun init_scoresheet :void
    ((var sht (:pointer (:struct scoresheet))))
  (for (var i :uchar 0) (< i 6) (incf i)
       (setf (aref (pmember sht :top)
                   i)
             NULLSCORE))
  (setf (pmember sht :tk) NULLSCORE)
  (setf (pmember sht :fk) NULLSCORE)
  (setf (pmember sht :fh) NULLSCORE)
  (setf (pmember sht :ss) NULLSCORE)
  (setf (pmember sht :ls) NULLSCORE)
  (setf (pmember sht :ch) NULLSCORE)
  (setf (pmember sht :y) NULLSCORE))

(defclcfun complete_scoresheet :uchar
    ((var sht (:pointer (:struct scoresheet))))
  (for (var i :uchar 0) (< i 6) (incf i)
       (when (= (aref (pmember sht :top)
                      i)
                NULLSCORE)
         (return 0)))
  (when (or (= (pmember sht :tk) NULLSCORE)
            (= (pmember sht :fk) NULLSCORE)
            (= (pmember sht :fh) NULLSCORE)
            (= (pmember sht :ss) NULLSCORE)
            (= (pmember sht :ls) NULLSCORE)
            (= (pmember sht :ch) NULLSCORE)
            (= (pmember sht :y) NULLSCORE))
    (return 0))
  (return 1))

;; assumes scoresheet is complete
(defclcfun score :ushort
    ((var sht (pointer (:struct scoresheet))))
  (var tmp :ushort)
  (for (var i :uchar 0)
       (< i 6)
       (incf i)
       (setf tmp
             (+ tmp
                (* (1+ i)
                   (aref (pmember sht :top)
                         i)))))
  (setf tmp (+ tmp
               (? (>= tmp 63)
                  35
                  0)))
  (setf tmp
        (+ tmp
           (? (> tmp 0)
              (? (> tmp 1)
                 (+ (* (1- tmp) 100)
                    50)
                 50)
              0)))
  (setf tmp (+ (pmember sht :tk)
               (pmember sht :fk)
               (pmember sht :fh)
               (pmember sht :ss)
               (pmember sht :ls)
               (pmember sht :ch)
               (? (> (pmember sht :y) 0)
                  (- (* (pmember sht :y) 100) 50)
                  0)))
  ;; This is the safe way:
  #|(if (complete_scoresheet sht)
        (return tmp)
        (return 0))|#
  ;; This is the fast way:
  (return tmp))

;; Here 0-5 are the values rather than 1-6 for convenience
(defclcfun throwdie :uchar ()
  (return (mod (pcg32) 6)))

(defclcfun throwdice :void
    ((var dice (pointer :uchar))
     (var throw (pointer :uchar)))
  (for (var i :uchar 0) (< i 6) (incf i)
       (when (aref throw i)
         (setf (aref dice i)
               (throwdie)
               ;; 5
               ))))

;; count dice types
;;
;; Note: assumes counts have been initialized
(defclcfun countdice :void
    ((var dice (pointer :uchar))
     (var counts (pointer :uchar)))
  (for (var i :uchar 0) (< i 5) (incf i)
       (incf (aref counts
                   (aref dice i)))))
  
;; find most popular dice value
(defclcfun mostpopular :void
    ((var dice (pointer :uchar))
     (var index (pointer :uchar))
     (var count (pointer :uchar)))
  (vararray counts :uchar (6)
            0 0 0 0 0 0)
  (countdice dice counts)
  (setf (value count) 0)
  (setf (value index) 0)
  (for (var i :uchar 0) (< i 6) (incf i)
       (when (> (aref counts i)
                (value count))
         (setf (value count) (aref counts i))
         (setf (value index) i))))

;; sum of dice values
(defclcfun sumofdice :uchar
    ((var dice (:pointer :uchar)))
  (var result :uchar 5) ; handling the 0-5 scheme
  (for (var i :uchar 0) (< i 5) (incf i)
       (setf result (+ result (aref dice i))))
  (return result))

;; pattern matching
(defclcfun fullhouse :uchar
    ((var dice (:pointer :uchar)))
  (vararray counts :uchar (6)
            0 0 0 0 0 0)
  (countdice dice counts)
  (var nonzeroes :uchar 0)
  (for (var i :uchar 0) (< i 6) (incf i)
       (when (not (zerop (aref counts i)))
         (incf nonzeroes)
         (when (> nonzeroes 2)
           (return 0))))
  (for (var i :uchar 0) (< i 0) (incf i)
       (when (and (not (= (aref counts i) 3))
                  (not (= (aref counts i) 2)))
         (return 0)))
  (return nonzeroes))

(defclcfun largestraight :uchar
    ((var dice (:pointer :uchar)))
  (vararray counts :uchar (6)
            0 0 0 0 0 0)
  (countdice dice counts)
  (return
    (or (and (aref counts 0)
             (aref counts 1)
             (aref counts 2)
             (aref counts 3)
             (aref counts 4))
        (and (aref counts 1)
             (aref counts 2)
             (aref counts 3)
             (aref counts 4)
             (aref counts 5)))))

(defclcfun smallstraight :uchar
    ((var dice (:pointer :uchar)))
  (vararray counts :uchar (6)
            0 0 0 0 0 0)
  (countdice dice counts)
  (return
    (or (and (aref counts 0)
             (aref counts 1)
             (aref counts 2)
             (aref counts 3))
        (and (aref counts 1)
             (aref counts 2)
             (aref counts 3)
             (aref counts 4))
        (and (aref counts 2)
             (aref counts 3)
             (aref counts 4)
             (aref counts 5)))))

(defclcfun sumdicetype :uchar
    ((var dice (:pointer :uchar))
     (var dicetype :uchar))
  (var result :uchar 0)
  (for (var i :uchar 0) (< i 5) (incf i)
       (when (= (aref dice i) dicetype)
         (incf result)))
  (return (* result (1+ dicetype))))

(defclcfun scoreoptimal :void
    ((var sht (:pointer (:struct scoresheet)))
     (var dice (:pointer :uchar)))
  (var mp :uchar)
  (var mpv :uchar)
  (mostpopular dice
               (address mp)
               (address mpv))
  ;; Yahtzee
  (when (= mpv 5)
    (if (= (pmember sht :y)
           NULLSCORE)
        (progn
          (setf (pmember sht :y)
                1)
          (return))
        (progn
          (incf (pmember sht :y))
          ;; Pick joker bonus
          (when (not (= (aref (pmember sht :top) mp)
                        NULLSCORE))
            (when (= (pmember sht :ls) NULLSCORE)
              (setf (pmember sht :ls) 40)
              (return))
            (when (= (pmember sht :ss) NULLSCORE)
              (setf (pmember sht :ss) 30)
              (return))
            (when (= (pmember sht :fh) NULLSCORE)
              (setf (pmember sht :fh) 25)
              (return))
            (when (= (pmember sht :fk) NULLSCORE)
              (setf (pmember sht :fk)
                    (sumofdice dice))
              (return))
            (when (= (pmember sht :tk) NULLSCORE)
              (setf (pmember sht :tk)
                    (sumofdice dice))
              (return))
            (when (= (pmember sht :ch) NULLSCORE)
              (setf (pmember sht :ch)
                    (sumofdice dice))
              (return))))))
  ;; both non-joker and non-yahtzee terminate here
  ;; top has priority
  (when (and (= (aref (pmember sht :top) mp)
                NULLSCORE)
             (>= mpv 3))
    (setf (aref (pmember sht :top) mp)
          mpv)
    (return))
  (when (and (largestraight dice)
             (= (pmember sht :ls) NULLSCORE))
    (setf (pmember sht :ls) 40)
    (return))
  (when (and (smallstraight dice)
             (= (pmember sht :ss) NULLSCORE))
    (setf (pmember sht :ss) 30)
    (return))
  (when (and (fullhouse dice)
             (= (pmember sht :fh) NULLSCORE))
    (setf (pmember sht :fh) 25)
    ;; debug
    ;; (setf (pmember sht :fhval)
    ;;       (fullhouse dice))
    ;; (for (var k :uchar 0) (< k 5) (incf k)
    ;;      (setf (aref (pmember sht :fhdice) k)
    ;;            (aref dice k)))
    ;; end debug
    (return))
  (when (and (>= mpv 4)
             (= (pmember sht :fk) NULLSCORE))
    (setf (pmember sht :fk)
          (sumofdice dice))
    (return))
  (when (and (>= mpv 3)
             (= (pmember sht :tk) NULLSCORE))
    (setf (pmember sht :tk)
          (sumofdice dice))
    (return))
  (when (= (pmember sht :ch) NULLSCORE)
    (setf (pmember sht :ch)
          (sumofdice dice))
    (return))
  ;; unfortunately we have to zero something
  (when (= (pmember sht :fk) NULLSCORE)
    (setf (pmember sht :fk)
          0)
    (return))
  (when (= (pmember sht :ls) NULLSCORE)
    (setf (pmember sht :ls)
          0)
    (return))
  (when (= (pmember sht :fh) NULLSCORE)
    (setf (pmember sht :fh)
          0)
    (return))
  (when (= (pmember sht :ss) NULLSCORE)
    (setf (pmember sht :ss)
          0)
    (return))
  (when (= (pmember sht :tk) NULLSCORE)
    (setf (pmember sht :tk)
          0)
    (return))
  (when (= (aref (pmember sht :top) 0) NULLSCORE)
    (setf (aref (pmember sht :top) 0)
          (sumdicetype dice 0))
    (return))
  (when (= (aref (pmember sht :top) 1) NULLSCORE)
    (setf (aref (pmember sht :top) 1)
          (sumdicetype dice 1))
    (return))
  (when (= (aref (pmember sht :top) 2) NULLSCORE)
    (setf (aref (pmember sht :top) 2)
          (sumdicetype dice 2))
    (return))
  (when (= (aref (pmember sht :top) 3) NULLSCORE)
    (setf (aref (pmember sht :top) 3)
          (sumdicetype dice 3))
    (return))
  (when (= (aref (pmember sht :top) 4) NULLSCORE)
    (setf (aref (pmember sht :top) 4)
          (sumdicetype dice 4))
    (return))
  (when (= (aref (pmember sht :top) 5) NULLSCORE)
    (setf (aref (pmember sht :top) 5)
          (sumdicetype dice 5))
    (return))
  (when (= (pmember sht :y) NULLSCORE)
    (setf (pmember sht :y) 0)
    (return)))

;; Yahtzee chaser strategy: Keep throwing for Yahtzee and score the
;; most points based on the final dice state.
(defclcfun ychase :void
    ((var sht (pointer (:struct scoresheet))))
  (vararray dice :uchar (5))
  ;; initially throw all dice
  (vararray throw :uchar (5)
            1 1 1 1 1)
  (for (var i :uchar 0) (< i 3) (incf i)
       ;; keep most popular and rethrow the rest
       (throwdice dice throw)
       (var mp :uchar)
       (var mpv :uchar)
       (mostpopular dice
                    (address mp)
                    (address mpv))
       (when (= mpv 5)
         (scoreoptimal sht dice)
         (return))
       (when (> mpv 1)
         (for (var j :uchar 0) (< j 5) (incf j)
              (when (= (aref dice j) mp)
                (setf (aref throw j) 0)))))
  (scoreoptimal sht dice))

;; Run the ychase strategy for an entire game and return total score
(defclcfun ychasegame :ushort
    ()
  (var sht (:struct scoresheet))
  (init_scoresheet (address sht))
  (for (var i :uchar 0) (< i 13) (incf i)
       (ychase (address sht)))
  (return (score (address sht))))

(defclckernel yahtzee
    ((var n (global (pointer :ulong)))
     (var seed (global (pointer :ulong)))
     (var scores (global (pointer :ushort))))
  (var gid (const :ulong)
       (get-global-id 0))
  (var nn (const :ulong)
       (value n))
  (when (< gid nn)
    (pcg32_init (+ (value seed)
                   gid))
    (setf (aref scores gid)
          (ychasegame))))

(defclckernel yahtzeehist
    ((var n (global (pointer :ulong)))
     (var seed (global (pointer :ulong)))
     (var hist (global (pointer :ulong))))
  (var gid (const :ulong)
       (get-global-id 0))
  (var nn (const :ulong)
       (value n))
  (when (< gid nn)
    (pcg32_init (+ (value seed)
                   gid))
    (atom_inc (address (aref hist (ychasegame))))))

(defun yahtzee (ngames)
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((nbuf
                (cl-create-buffer context
                                  :type :ulong
                                  :data (list ngames)))
               (sbuf
                (cl-create-buffer context
                                  :type :ulong
                                  :data
                                  (list
                                   (mod (* (get-internal-real-time)
                                           (get-internal-real-time))
                                        (expt 2 64)))))
               (rbuf
                (cl-create-buffer context
                                  :type :ushort
                                  :count ngames))
               (program
                (let* ((p
                        (cl-create-program-with-source
                         context
                         (program-source-from-kernels yahtzee))))
                  (cl-build-program-with-log p (list dev))
                  p))
               (kernel
                (cl-create-kernel program "yahtzee")))
          (cl-set-kernel-arg kernel 0 :value nbuf)
          (cl-set-kernel-arg kernel 1 :value sbuf)
          (cl-set-kernel-arg kernel 2 :value rbuf)
          (let* ((result
                  (first
                   (last
                    (cl-wait-and-release-events
                     (list (cl-enqueue-kernel queue kernel ngames)
                           (cl-enqueue-read-buffer
                            queue rbuf :ushort ngames)))))))
            (cl-release-kernel kernel)
            (cl-release-program program)
            (mapcar #'cl-release-mem-object
                    (list rbuf sbuf nbuf))
            result))))))

;; utility for yahtzee-hist
(defun zip (x y)
  (mapcar #'cons x y))

;;;; Main result: Histogram generator for yahtzee-chaser strategy
(defun yahtzee-hist (ngames)
  (let* ((plat (first (cl-get-platform-ids)))
         (dev (first (cl-get-device-ids
                      plat
                      +CL-DEVICE-TYPE-ALL+))))
    (with-opencl-context (context plat (list dev))
      (with-opencl-command-queue (queue context dev)
        (let* ((nbuf
                (cl-create-buffer context
                                  :type :ulong
                                  :data (list ngames)))
               (sbuf
                (cl-create-buffer context
                                  :type :ulong
                                  :data
                                  (list
                                   (mod (* (get-internal-real-time)
                                           (get-internal-real-time))
                                        (expt 2 64)))))
               (rbuf
                (cl-create-buffer context
                                  :type :ulong
                                  :count (1+ *maxscore*)))
               (program
                (let* ((p
                        (cl-create-program-with-source
                         context
                         (concatenate
                          'string
                          (format nil "#pragma OPENCL EXTENSION cl_khr_int64_base_atomics : enable~%")
                          (program-source-from-kernels yahtzeehist)))))
                  (cl-build-program-with-log p (list dev))
                  p))
               (kernel
                (cl-create-kernel program "yahtzeehist")))
          (cl-set-kernel-arg kernel 0 :value nbuf)
          (cl-set-kernel-arg kernel 1 :value sbuf)
          (cl-set-kernel-arg kernel 2 :value rbuf)
          (let* ((result
                  (first
                   (last
                    (cl-wait-and-release-events
                     (list (cl-enqueue-kernel queue kernel ngames)
                           (cl-enqueue-read-buffer
                            queue rbuf :ulong (1+ *maxscore*))))))))
            (cl-release-kernel kernel)
            (cl-release-program program)
            (mapcar #'cl-release-mem-object
                    (list rbuf sbuf nbuf))
            result))))))

(defun draw-yahtzee-hist (ngames)
  (let* ((rawhist (time (yahtzee-hist ngames)))
         (hist (zip (loop
                       for i from 0 to *maxscore*
                       collecting i)
                    (map 'list #'identity
                         rawhist)))
         (maxscore
          (let* ((r 0))
            (loop
               for i to *maxscore*
               when (not (zerop (aref rawhist i)))
               do (setf r i))
            r))
         (minscore
          (let* ((r 0))
            (loop
               for i from *maxscore* downto 0
               when (not (zerop (aref rawhist i)))
               do (setf r i))
            r))
         (meanscore (let* ((ngames 0)
                           (totalscore 0))
                      (loop
                         for i to *maxscore*
                         do
                           (incf ngames
                                 (aref rawhist i))
                           (incf totalscore
                                 (* (aref rawhist i)
                                    i)))
                      (/ (float totalscore 1d0)
                         ngames))))
    (draw
     (page (list
            (plot2d (list
                     (line hist
                           :style "boxes"
                           :color "red"
                           :title "yahtzee score histogram"))
                    :logaxes (list "y")))))
    (list :min minscore
          :mean meanscore
          :max maxscore)))
