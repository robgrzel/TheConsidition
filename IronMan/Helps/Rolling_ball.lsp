;;; Rolling_ball.lsp --- Program to find the mid-boundary of two
;;;                      not smoothed or curved, quadratic or cubic
;;;                      smoothed 2D polylines.
;;;                      Using the rolling ball theory.
;; Written in plain AutoLISP, tested with AutoCAD release 14 and 2002.
;; Comments, bug reports and improvements are welcome.

;; v7.02 - Time-stamp: <2005-12-16 11:51:26 gb>
;; - Some minor changes, correction of some comments,
;; - Improved calculation of ball increase and
;;   decrease factor, more speed.
;; - When polylines are closed, process will start
;;   at the polyline points that are closest to the
;;   pick point on 1. boundary.

;; v7.01 - Time-stamp: <2005-12-06 22:12:04 gb>
;; - Some minor changes, correction of some comments.
;; - Fixed a bug.

;; v7.00 - Time-stamp: <2005-11-30 00:10:09 gb>
;; - Some minor changes.
;; - Fixed a bug.

;; v6.04 - Time-stamp: <2005-11-24 09:32:05 gb>
;; - Revised method for getting the side
;;   where to draw the mid-boundary.
;; - Some minor changes.
;; - Fixed a bug.

;; v6.03 - Time-stamp: <2005-09-25 19:09:01 gb>
;; - Revised method for `area sweeping' for total
;;   included line angles < 180 degrees.
;; - Fixed a bug.

;; v6.02 - Time-stamp: <2005-09-21 09:26:23 gb>
;; - Command VERTCNT now marks also the Vertices in a single polyline.
;; - Fixed a bug.

;; v6.01 - Time-stamp: <2005-09-21 09:26:23 gb>
;; - Fixed a bug.

;; v6.00 - Time-stamp: <2005-09-20 09:06:50 gb>
;; - Added code to be able to influence the resolution of polyline arcs.
;; - Revised the code of building the list of segment points
;;   and there ball angles.
;; - Fixed a bug.

;; v5.43 - Time-stamp: <2005-08-15 14:01:54 gb>
;; - Exchanged sort function with a Quick sort solution.

;; v5.42 - Time-stamp: <2005-08-08 08:55:13 gb>
;; - Fixed a bug and revised a few functions.
;; - Exchanged sort function with a faster one.

;; v5.40 - Time-stamp: <2005-08-02 06:24:41 gb>
;; - Fixed a bug and revised a few functions.

;; v5.39 - Time-stamp: <2005-07-24 23:09:50 gb>
;; - Fixed a bug.

;; v5.37 - Time-stamp: <2005-07-22 15:11:11 gb>
;; - Revised calculation of ball increase/decrease.

;; v5.36 - Time-stamp: <2005-06-11 16:49:46 gb>
;; - Revised calculation of ball increase/decrease.

;; v5.35 - Time-stamp: <2005-06-08 13:42:26 gb>
;; - Revised calculation of ball increase, fixed some bugs
;;   and improved  method for correcting center and
;;   intersection point on 2. border.
;;   The program is now also able to process polylines
;;   that are smoothed quadratic or cubic.

;;; Commentary:

;;   Original program `Rollin.lsp' created by John Leavines
;; for JefferyPSanders.com, modified by Guenther Bittner.

;; The rolling ball theory:  The mid-boundary should be a line running
;; from center to center of the largest ball possible rolling between
;; two boundaries.

;; THIS PROGRAM IS PROVIDED "AS IS" AND WITH ALL FAULTS.  THE AUTHOR
;; SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR
;; FITNESS FOR A PARTICULAR USE.  THE AUTHOR ALSO DOES NOT WARRANT THAT
;; THE OPERATION OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.

;; Special thanks to:
;;   - the author's of "Inside AutoLisp" for rel. 10 published
;;     by New Riders Publications for their functions UDIST, UKWORD and UREAL.
;;   - R. Robert Bell for his cross between (entsel) & (ssget), function I:EntSelF.
;;   - Serge Pashkov for his function STD-SPLIT-LIST.
;;   - Duff Kurland - Autodesk, Inc.   August 22, 1986 for his function ROUND.

;;; Code:

;; All function names are prefixed with `F:rb-', local functions with `#'
;; All global variable names are prefixed with `*' or `**'

;;;-------------------------------------------------------------------
;;;
;;;                      SUB FUNCTIONS
;;;
;;;-------------------------------------------------------------------
;; Radian to degree
(defun F:rb-RTD (r)  (/ (* r 180.0) pi))

;;; DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG >
;;; DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG >
;;; DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG >

;; Debugging functions
;;--------------------------------
(defun C:BREAK ()
  (setq *BREAK* (not *BREAK*))          ; switch it on and off
  (cond
    (*BREAK*       (princ "\n BREAK is ON" ) )
    ((not *BREAK*) (princ "\n BREAK is OFF") )
    )
  (princ)
  )
;;--------------------------------
(defun break (s)
  (if *BREAK*
    (progn
      (princ "\nBREAK>> (stop with <Enter>, type <cont> to switch off break)\nBREAK>> ")
      (princ s)
      (while (and *BREAK* (/= (setq s (getstring "\nBREAK>> ")) ""))
        (if (= (strcase s) "CONT")
          (CONT)
          (print (eval (read s))))))))
;;--------------------------------
(defun cont () (setq *BREAK* nil)) ; continue without any interruption
;;--------------------------------
(defun dbg-print (s)                    ; accepts atoms and lists
  (if *DEBUG*
    (if (listp s)
      (mapcar 'print s)
      (print s)
      )))
;;--------------------------------
(defun C:DEBUG ()
  (setq *DEBUG* (not *DEBUG*))          ; switch it on and off
  (cond
    (*DEBUG*       (setq **ZOOM* 'T) (princ "\n DEBUG is ON" ))
    ((not *DEBUG*) (setq **ZOOM* nil)(princ "\n DEBUG is OFF"))
    )
  (princ)
  )
;;--------------------------------
(defun C:DEBUG2 ()
  (setq *DEBUG2* (not *DEBUG2*))        ; switch it on and off
  (cond
    (*DEBUG2*       (setq *BREAK* 'T) (princ "\n DEBUG2 is ON" ))
    ((not *DEBUG2*) (setq *BREAK* nil)(princ "\n DEBUG2 is OFF"))
    )
  (princ)
  )
;;--------------------------------
(defun C:FLAG ()                        ; set a mark for anything
  (setq **FLAG* (not **FLAG*))          ; switch it on and off
  (cond
    (**FLAG*       (princ "\n FLAG is ON" ) )
    ((not **FLAG*) (princ "\n FLAG is OFF") )
    )
  (princ)
  )
;;--------------------------------
(defun C:ZOOMON ()                      ; switch `ZOOM-2-CIRCLE' on
  (setq **ZOOM* 'T) (princ "\n ZOOM is ON" ) (princ)
  )
(defun C:ZOOMOFF ()                     ; switch `ZOOM-2-CIRCLE' off
  (setq **ZOOM* nil)  (princ "\n ZOOM is OFF" ) (princ)
  )
;;--------------------------------
(defun C:RUNBREAK ( / n)
  (setq n (getreal
           (strcat "\n Enter RUN number where to break the program <"
                   (itoa (fix (getvar "USERR5")))
                   ">: "
                   )
           )
        )
  (if n (setvar "USERR5" (fix n)))
  (princ "\n Break at: ")
  (princ (fix (getvar "USERR5")))
  (princ)
  )
;;--------------------------------
;; Move all objects on selected layer to the back of draworder
(defun C:LAYDRAW ( / -ENT -LAY -SS)
  (setq -ent (entsel "\Select layer: "))
  (if -ent
    (progn
      (setq -lay (list (assoc 8 (entget (car -ent)))))
      (setq -ss (ssget "_X" -lay))
      (command "_draworder" -ss "" "_b")
      )
    )
  (princ)
  )
;;--------------------------------
;; MAKE PROGRAMS WORKING VISIBLE
;;
(defun F:rb-DEBUG ( program varlst / bivec botex col cpt cpt-new
                            d_cpt_ipt flag int1 int2 int2-new
                            lay1 lay2 lay3 lay4 leftex line lst n
                            num oc ol pix_ratio ppl pt1 pt2 pt3 pt4 px
                            py r1 r2 rad rightex runlth scrhgt scrwid
                            segline1 segline2 tmp topex xmax xmin
                            xy_ratio x_cen x_dim ymax ymin y_cen y_dim
                            ;; local functions
                            #ZOOM-2-CIRCLE #WINDOWP
                            #GET-SCREEN-CORNERS
                            )

  ;;------------------------------------------------------------------
  ;; LOCAL FUNCTIONS
  ;;------------------------------------------------------------------
  (defun #GET-SCREEN-CORNERS ( / BOTEX LEFTEX PIX_RATIO PT1 PT2 PT3 PT4 RIGHTEX
                                 SCRHGT SCRWID TOPEX UC XY_RATIO X_CEN X_DIM Y_CEN Y_DIM
                                 )

    (setq PIX_RATIO 1.0 ; set PIX_RATIO to horizontal/vertical dot pitch ratio of monitor
          X_CEN     (car (getvar "VIEWCTR")) ; get X value of viewport center
          Y_CEN     (cadr (getvar "VIEWCTR")) ; get Y value of viewport center
          SCRHGT    (getvar "VIEWSIZE") ; get height of viewport in dwg units
          X_DIM     (car (getvar "SCREENSIZE")) ; get height of viewport in pixels
          Y_DIM     (cadr (getvar "SCREENSIZE")) ; get width of viewport in pixels
          XY_RATIO  (/ X_DIM Y_DIM) ; calculate width:height ratio of viewport
          SCRWID    (* SCRHGT XY_RATIO PIX_RATIO) ; calculate width of viewport in dwg units
          LEFTEX    (- X_CEN (/ SCRWID 2.0)) ; \  calculate left,
          RIGHTEX   (+ X_CEN (/ SCRWID 2.0)) ;  \ right,
          BOTEX     (- Y_CEN (/ SCRHGT 2.0)) ;  / bottom and
          TOPEX     (+ Y_CEN (/ SCRHGT 2.0)) ; /  top extents of viewport
          PT1 (list LEFTEX  BOTEX 0.0) ; bottom left  corner of viewport
          PT2 (list LEFTEX  TOPEX 0.0) ;    top left  corner of viewport
          PT3 (list RIGHTEX TOPEX 0.0) ;    top right corner of viewport
          PT4 (list RIGHTEX BOTEX 0.0) ; bottom right corner of viewport
          )
    (list pt1 pt3)
    )

  ;; (#WINDOWP <point> <corner1> <corner2> )
  ;; Returns NON-NIL if <point> lies on or within the rectangular
  ;; extents defined by the <corner1> and <corner2>, two points on
  ;; either diagonal of the subject rectangle.
  ;;
  (defun #WINDOWP ( p c1 c2 / px py xmax xmin ymax ymin )
    (setq xmax (max (car c1) (car c2))  ; high X
          xmin (min (car c1) (car c2))  ; low X
          ymax (max (cadr c1) (cadr c2)) ; high Y
          ymin (min (cadr c1) (cadr c2)) ; low Y
          px   (car p)                  ; X and Y of point
          py   (cadr p)                 ; to examine.
          )
    (and (>= xmax px xmin)              ; both conditions must be TRUE
         (>= ymax py ymin)         ; if the point lies in the extents.
         )                             ; The result of AND becomes the
    )                                   ; result of #WINDOWP.

  ;; ZOOM to circle
  (defun #ZOOM-2-CIRCLE (cpt rad fac )
    (if **ZOOM*
      (command "_zoom" "_w"
               (polar cpt       (/ PI 4)  (* rad fac))
               (polar cpt (+ pi (/ PI 4)) (* rad fac))
               )
      )
    )
  ;;------------------------------------------------------------------ end of local functions

  (if *DEBUG*
    (cond
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß 1
      ;;ßßßßßßßßßßßßßßßßßß     F:rb-DEBUG 1     ßßßßßßßßßßßßßßßßßß
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
      ((= program 1)
        (setq segline1 (nth 0 varlst))
        (setq segline2 (nth 1 varlst))
        (setq bivec    (nth 2 varlst))
        (setq int1     (nth 3 varlst))
        (setq cpt      (nth 4 varlst))
        (setq rad      (nth 5 varlst))
        (setq cpt-new  (nth 6 varlst))
        (setq int2-new (nth 7 varlst))
        (setq flag     (nth 8 varlst))
        (setq col      (nth 9 varlst))

       (redraw)

       (if **FLAG* (setq flag 7))

       (if (<= rad 0)(setq rad 1))

       (setq lay1 (strcat "RB-" (itoa col) "-Debug-" (itoa flag) "-Radius" )
             lay2 (strcat "RB-" (itoa col) "-Debug-" "100" "-Center" )
             lay3 (strcat "RB-" (itoa col) "-Debug-" "Round-off-Err" )
             lay4 (strcat "RB-" (itoa col) "-Debug-" "Dir-int-int2" )
             )

       (#ZOOM-2-CIRCLE cpt rad 2.5)
       ;;-----------------------------------------------
       (F:rb-SET-LAYER lay1 (if (< 130 flag 140)
                              220
                              flag ) 'T )

       ;; draw radius lines
       (if (> (distance int1     cpt-new) 1.0e-8) (command "_line" int1     cpt-new  ""))
       (if (> (distance int2-new cpt-new) 1.0e-8) (command "_line" int2-new cpt-new  ""))

       (if (and **PREV-CPT* (not (equal **PREV-CPT* cpt-new 1e-8))) ;; DEBUG
         (progn
           (F:rb-SET-LAYER lay2 100 'T)
           ;; draw centerline
           (if (> (distance **PREV-CPT* cpt-new) 1.0e-8)
             (command "_line" **PREV-CPT* cpt-new  "")
             )
           )
         )
       ;;-----------------------------------------------

       ;; calculate circle radius
       (setq r1 (distance int1    cpt-new ))
       (setq r2 (distance cpt-new int2-new))

       ;; CHECK THE LENGTH OF BOTH RADIUS LINES
       (cond
         ((and
            (< 100 flag 300) ; flag = reflects method that found the intersection
            (not (equal r1 r2 1e-6))    ; check radius
            )

           ;; count errors
           (setvar "USERS5"
                   (itoa
                    (1+ (atoi (getvar "USERS5")))
                    ))

           ;;(setvar "LOGFILEMODE" 1)

           (dbg-print
            (list
             ;; Color flag  = number of center correcting method and their color
             ;; centerpoint = circle center point
             ;; "USERS5"    = counter
             (list 'Color flag ', 'Centerpoint: (strcat (rtos (car cpt-new)) ","
                                                        (rtos (cadr cpt-new))))
             (list (read (strcat (getvar "USERS5")":")) 'Radius_int1_cpt:  (rtos r1 2 7))
             (list (read (strcat (getvar "USERS5")":")) 'Radius_int2_cpt:  (rtos r2 2 7)
                   'Diff: (rtos (- (max r1 r2)(min r1 r2)) 2 7) )
             '===================================================== ; separator
             'end
             )
            )

           ;;(setvar "LOGFILEMODE" 0)

           (F:rb-SET-LAYER lay3 51 'T)

           ;; set mark when radius lines are slightly different
           (command "_circle" cpt-new (F:rb-PIX2UNITS 2.0))
           )
         )

       (if **ZOOM* (F:rb-GRDRAW-POINT int1 1 "circle"))
       (grdraw cpt-new int1     7 -1)   ; line circle center hit pt. 1
       (grdraw cpt-new int2-new 7 -1)   ; line circle center hit pt. 2

       ;; mark current segments
       (apply 'grdraw (append segline1 '(7 -1)))
       (apply 'grdraw (append segline2 '(7 -1)))

       ;;-----------------------------------------------
       (if (F:rb-EVERY 'and bivec)
         (progn
           (setq px (polar cpt-new (apply 'angle bivec) r1))
           ;; draw bisecting line
           (grdraw cpt-new px flag -2 )
           ;; show angle direction of bisecting line
           (F:rb-GRDRAW-ARROW cpt-new (apply 'angle bivec) (F:rb-ARROW-LEN) col)
           )
         )
       ;;-----------------------------------------------
       (break (strcat "R1: " (rtos r1 2 5) " - R2:" (rtos r2 2 5)) )

       )
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß 2
      ;;ßßßßßßßßßßßßßßßßßß     F:rb-DEBUG 2     ßßßßßßßßßßßßßßßßßß
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
      ((= program 2)
        (setq lst (nth 0 varlst))
        (setq pt1 (nth 1 varlst))
        (setq pt2 (nth 2 varlst))

        (if pt1
          (progn
            ;; make line layer
            (F:rb-SET-LAYER (strcat "RB-" **COL1* "-Debug-AreaSw_less_180-1") 1 'T)
            (command "_circle" pt1 (F:rb-PIX2UNITS 0.125))
            )
          )
        (if pt2
          (progn
            ;; make line layer
            (F:rb-SET-LAYER (strcat "RB-" **COL1* "-Debug-AreaSw_less_180-2") 2 'T)
            (command "_circle" pt2 (F:rb-PIX2UNITS 0.125))
            )
          )
        (while lst
          ;; make line layer
          (F:rb-SET-LAYER (strcat "RB-" **COL1* "-Debug-AreaSw_less_180-3") 253 'T)
          (if (and (car lst) (cadr lst) (> (distance (car lst) (cadr lst)) 0))
            (command "_line" (car lst) (cadr lst) "")
            )
          ;; make circle layer
          (F:rb-SET-LAYER (strcat "RB-" **COL1* "-Debug-AreaSw_less_180-1") 1 'T)
          (if (cadr lst) (command "_circle" (cadr lst) (F:rb-PIX2UNITS 0.0625)))
          ;; make circle layer
          (F:rb-SET-LAYER (strcat "RB-" **COL1* "-Debug-AreaSw_less_180-2") 2 'T)
          (if (car lst) (command "_circle" (car lst) (F:rb-PIX2UNITS 0.0625)))
          (setq lst (cddr lst) )

          ;;(break "AreaSw")
          )
        )
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß 3
      ;;ßßßßßßßßßßßßßßßßßß     F:rb-DEBUG 3     ßßßßßßßßßßßßßßßßßß
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
      ((= program 3)
        (setq int1     (nth 0 varlst))
        (setq cpt      (nth 1 varlst))
        (setq int2     (nth 2 varlst))
        (setq segline1 (nth 3 varlst))
        (setq segline2 (nth 4 varlst))

        ;;(break "sr3")
        (setq ol (getvar "clayer")
              oc (getvar "cecolor")
              )
        (F:rb-SET-LAYER (strcat "RB-" **COL1* "-Debug-error_int1-cpt-int2") 51 'T)
        (if (> (distance int1 cpt) 1e-8) (command "_line" int1 cpt ""))
        (if (> (distance int2 cpt) 1e-8) (command "_line" int2 cpt ""))
        (setvar "cecolor" "9")
       (command "_line" (car segline1) (cadr segline1) "")
       (command "_line" (car segline2) (cadr segline2) "")
       (setvar "clayer"  ol)
       (setvar "cecolor" oc)
       )
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß 4
      ;;ßßßßßßßßßßßßßßßßßß     F:rb-DEBUG 4     ßßßßßßßßßßßßßßßßßß
      ;;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
      ((and (= program 4) *debug2*)
        (setq cpt  (nth 0 varlst))
        (setq rad  (nth 1 varlst))
        (setq int2 (nth 2 varlst))
        (setq line (nth 3 varlst))
        (setq ppL  (nth 4 varlst))

        (if int2
          (progn
            (setq tmp (#GET-SCREEN-CORNERS))
            (if (not (#WINDOWP cpt  (car tmp) (cadr tmp)))
              (command "_zoom" "_c" cpt "")
              )
            (if (not (#WINDOWP int2 (car tmp) (cadr tmp)))
              (command "_zoom" "_w"
                       (polar cpt       (/ PI 4)  (* rad 2))
                       (polar cpt (+ pi (/ PI 4)) (* rad 2))
                       )
              )
            (redraw)
            (F:rb-GRDRAW-POINT cpt 7 "X")
            (grdraw cpt ppL  8 -1)
            (apply 'grdraw (append line '(8 -1)))
            (cond
              ((equal int2 (car  line) 0) (grdraw cpt int2 1 -1) )
              ((equal int2 (cadr line) 0) (grdraw cpt int2 2 -1) )
              ('T (grdraw cpt int2 7 -1) )
              )
            ;;(break "int2")
            )
          )
        )
      ;;----------------------------------------
      ('T (princ))
      )                                 ;_cond
    )
  )
;;;
;;; DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG <
;;; DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG <
;;; DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG <
;;;

;;;
;;;-------------------------------------------------------------------
;;;
;; Keeps all elements to which the predicate applies.
(defun F:rb-KEEP-IF (pred lst)
  (apply 'append
         (mapcar '(lambda (ele)
                   (if (apply pred (list ele)) (list ele))
                   )
                 lst
                 )))
;;;
;;;-------------------------------------------------------------------
;;;
;; Integer sequence including start and end element
;; (F:rb-ISEQ 0 1) => (0 1)
(defun F:rb-ISEQ (start end / lst)
  (repeat (1+ (- end start))
          (setq lst (cons end lst)
                end (1- end)
                )
          )
  lst
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 11.06.2005
;;
;; Get index numbers of segments behind and in front of `segline'
;; F:rb-GET-POSITION-NUMS args:
;;  1.  <start segment>
;;  2.  <number> of backward segments
;;  3.  <number> of forward segments
(defun F:rb-GET-POSITION-NUMS ( segline backward forward / POS )

  (if (not **BLIST-UNSPLIT-LEN*)

    ;; Undo split of main point list and keep it as global list
    ;; to avoid undoing splitting several times.
    ;; (F:rb-UNSPLIT-LIST '( ((p1 p2) (p2 p3) (p3 p4)) ...) => '( (p1 p2) (p2 p3) (p3 p4) ...)
    (setq **BLIST-UNSPLIT* (F:rb-UNSPLIT-LIST **BLIST-SPLIT*)

          **BLIST-UNSPLIT-LEN* (length **BLIST-UNSPLIT*) ; list length
          )
    )

  ;; Get the position number of the current segment in the list
  (if (setq pos (F:rb-POSITION segline **BLIST-UNSPLIT*)) ; pos = 'nth position, base is zero

    ;; return list of position numbers, example: pos = 4   => '(5 6 7 3 2 1)
    (append
     (if (> forward 0)
       (F:rb-KEEP-IF '(lambda (x) (< x **BLIST-UNSPLIT-LEN*))
                     (F:rb-ISEQ (1+ pos) (+ pos forward)) ; Integer SEQuence, start, end
                     )
       nil
       )
     (if (> backward 0)
       (reverse
        (F:rb-KEEP-IF '(lambda (x) (> x -1))
                      (F:rb-ISEQ (- pos backward) (1- pos)) ; Integer SEQuence, start, end
                      )
        )
       nil
       )
     )
    nil                         ; return 'nil if `segline' not in list
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Check whether three 3D points
;; are situated on the same straight line
;; [if yes, t will be returned; otherwise nil]
;;
(defun F:rb-COLINEAR ( p1 p2 p3 tol)
  (equal '(0.0 0.0 0.0) (F:rb-VECTOR-PRODUCT
                         (mapcar '- p2 p1)
                         (mapcar '- p3 p1)
                         )
         tol                            ; tolerance
         )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Vector product of two 3D vectors
;; [is always perpendicular to both vectors;
;;  returning a zero vector implies and is implied by
;;  both vectors being parallel]
(defun F:rb-VECTOR-PRODUCT (v1 v2)
  (list
   (- (* (cadr  v1) (caddr v2)) (* (caddr v1) (cadr  v2)))
   (- (* (caddr v1) (car   v2)) (* (car   v1) (caddr v2)))
   (- (* (car   v1) (cadr  v2)) (* (cadr  v1) (car   v2)))
   )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Remove colinear points from point list
(defun F:rb-REMOVE-COLINEAR-PTS (lst fuzz / cnt len lst1 lst2 pt1 tmp )

  ;; Removing colinear Center points
  (if (= "Y" (F:rb-UKWORD
              1
              "N Y"
              " Do you want to remove colinear points from centerline? [Yes/No] "
              "Y"
              )
         )
    (progn
      (F:rb-CLEAR-CMDLINE 80)
      (princ "\r   Removing colinear points from list ... ")
      (setq lst1 lst
            lst2 '()
            pt1  (car lst)
            cnt  0
            len (length lst)
            )

      (while (cdr lst1)
        ;;...........................
        (setq tmp (F:rb-SPIN tmp))
        (princ (strcat "\r " tmp))
        ;;...........................
        (cond
          ;; If there is a 3. point in list
          ((caddr lst1)
            (if (not (F:rb-COLINEAR (car lst1) (cadr lst1) (caddr lst1) fuzz ))
              (setq lst2 (cons (cadr lst1) lst2) )
              (setq cnt (1+ cnt))
              )
            )
          ;; Else process is finished and the last point is added to the new list
          ('T (setq lst2 (cons (last lst1) lst2)))
          )
        (setq lst1 (cdr lst1) )
        )

      (princ "\r   Removing colinear pts. from list ... DONE, removed: ")
      (princ cnt)
      (princ ", kept: ")
      (princ (- len cnt))

      (if (> cnt 0)
        (cons pt1 (reverse lst2))       ; return cleaned list
        lst                             ; return original, no change
        )

      )
    lst                                 ; return original, no change
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; (ROUND NUM FRAC)  returns NUM rounded to the nearest FRAC.
;; Examples:  (ROUND 15.2 0.25) returns 15.25
;;            (ROUND 15.1 0.25) returns 15.00
;; By Duff Kurland - Autodesk, Inc.   August 22, 1986
(defun F:rb-ROUND ( num frac / half over )
  (setq half (/ frac 2.0))
  (setq over (rem num frac))            ; Get remainder
  (if (>= over half)
    (+ num frac (- over))               ; Round up
    (- num over)                        ; Round down
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Clear the command line
(defun F:rb-CLEAR-CMDLINE (n / ch tmp)
  (setq tmp "\r" ch " ")
  (repeat n (setq tmp (strcat tmp ch)))
  (princ (strcat tmp "\r"))
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Sub-function to determine if a point `pt' lies
;; on the segment defined by `line'.
(defun F:rb-ONLINE (line pt)
  (equal (apply 'distance line)
         (+ (distance (car  line) pt)
            (distance (cadr line) pt)
            )
         1e-8
         )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Test if distance `basept'-`pt1' is equal distance `basept'-`pt2'.
(defun F:rb-EQUAL-DISTANCE ( basept pt1 pt2 fuzzy)
  (equal (distance basept pt1)(distance basept pt2) fuzzy)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Test if point `pt' is inside a circle,
;; `cpt' = circle center, `rad' = circle radius.
(defun F:rb-INSIDE-CIRCLE (pt cpt rad)
  (< (distance pt cpt) rad)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Calculate total included line angle, base point: `p0'
(defun F:rb-LTANGLE ( p0 p1 p2 / a1 a2 a3)
  (setq a1 (angle p0 p1)
        a2 (angle p0 p2)
        )
  (if (> a1 a2)
    (setq a3 (- (* pi 2) (- a1 a2)))
    (setq a3 (- a2 a1))
    )
  (list (cons "LEFT" (- (* pi 2) a3)) ; total included line angle `left-side'
        (cons "RIGHT" a3)      ; total included line angle `right-side'
        )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Total included angle of 2 angles
(defun F:rb-INC-ANG ( a1 a2 / IncA)
  (if (> a1 a2)
    (setq IncA (- (* pi 2) (- a1 a2)))
    (setq IncA (- a2 a1) )
    )
  (min IncA (- (* pi 2) IncA))
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Bisect an Angle
;; (F:rb-GET-BISECTION-ANGLE(getpoint"INT, ")(getpoint"1, ")(getpoint"2: "))
(defun F:rb-GET-BISECTION-ANGLE (vertex p1 p2 )
  (angle
   vertex
   (F:rb-MIDPT
    (polar vertex (angle vertex p1) 1.0)
    (polar vertex (angle vertex p2) 1.0)
    )
   )
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-CMDACTIVE-P ()
  (while (= 1 (logand (getvar "cmdactive") 1)) (command pause))
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 11.06.2005
;;
(defun F:rb-GET-INTERSECTION ( n segline2 cpt rad int1 int1->BallAng int2
                                 backward forward mode /
                                 BIVEC1 BIVEC2 CP CPT-NEW FLAG FUZZY INT2-NEW
                                 LST POSNUMS PP SEG1 SEG2
                                 )

  (setq seg1 (F:rb-MAKE-PERP-LINE int1 int1->BallAng rad) )

  (cond
    ((= mode "2")
      (setq lst   (list segline2)
            fuzzy 2
            )
      )
    ((= mode "3")
      (setq ;; Get index numbers of segments behind
            ;; and in front of `segline2'
            ;;
            ;; F:rb-GET-POSITION-NUMS args:
            ;;  1.  <start segment>
            ;;  2.  <number> of backward segments
            ;;  3.  <number> of forward segments
            posnums (F:rb-GET-POSITION-NUMS segline2 backward forward )

            fuzzy 3
            )
      ;; get the first following segment of `segline2'
      (if posnums
        (setq lst     (list (nth (car posnums) **BLIST-UNSPLIT*))
              posnums (cdr posnums)     ; shorten the list
              )
        (setq lst (list segline2))
        )
      )
    ((= mode "4b")
      (setq lst   (list segline2)
            fuzzy 4
            )
      )
    )
  ;;------------------------------------------------------------------
  (while lst
    (setq seg2     (car lst)            ; segment of 2. border
          cpt-new  nil                  ; center point
          int2-new nil                  ; intersection point
          )

    ;; Calculate first point for getting a bisecting line of the segments.
    (if (setq bivec1 (apply 'inters (append seg1 seg2 '(nil)) ))
      (progn

        ;; Calculate second point for getting a bisecting line of the segments.
        (setq bivec2 (polar
                      bivec1
                      (F:rb-GET-BISECTION-ANGLE bivec1 int1 int2)
                      1.0
                      )

              ;; Calculate corrected circle center point on bisection line
              cpt-new (inters bivec1 bivec2 int1 int1->BallAng nil )
              )

        (if cpt-new
          ;; Point on 2. border perpendicular to the corrected circle center
          (setq int2-new (F:rb-GET-PERP-PT seg2 cpt-new) )
          )
        )
      )
    ;;----------------------------------------------------------------
    (if (F:rb-RESULT-OK int1 cpt rad seg2 cpt-new int2-new fuzzy)
      (setq flag 'T                     ; solution found
            lst  nil                    ; stop loop
            )
      (if posnums
        ;; get a previous or following segment
        (setq lst     (list (nth (car posnums) **BLIST-UNSPLIT*))
              posnums (cdr posnums)     ; shorten the list
              n       (1+ n)
              )
        (setq lst nil)                  ; stop loop
        )
      )                                 ;_ if
    ;;----------------------------------------------------------------
    )                                   ;_ while

  (list (if flag n nil) cpt-new int2-new (list bivec1 bivec2) )
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-BISECTING-VECTOR (line1 line2 / #VEC-NORM #VEC-ADD #VEC-SUBTR x )

  (defun #VEC-SUBTR (a b)               ; subtraction of two vectors
    (mapcar '- a b) )
  (defun #VEC-ADD (a b)                 ; addition of two vectors
    (mapcar '+ a b) )
  (defun #VEC-NORM (v / d)              ; normalize a vector
    (setq d (distance v '(0.0 0.0 0.0)))
    (mapcar '/ v (list d d d)) )

  (if (not (setq x (apply 'inters (append line1 line2 '(nil)) ) ))
    ;; lines are parallel or reverse
    (list (setq x (F:rb-MIDPT (car line1) (car line2)))
          (polar x (apply 'angle line1) 1.0)
          )
    (list x (#VEC-ADD x (F:rb-MIDPT
                         (#VEC-NORM (#VEC-SUBTR (cadr line1)(car line1)))
                         (#VEC-NORM (#VEC-SUBTR (cadr line2)(car line2)))
                         ))))
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; F:rb-POSITION returns the 'nth index of the first
;; element in the list or nil if not found.
;; (F:rb-POSITION 3 '(0 1 1 2)) => nil, (F:rb-POSITION 1 '(0 1 1 2)) => 1
(defun F:rb-POSITION (x lst / n)
  (if (not (zerop (setq n (length (member x lst)))))
    (- (length lst) n)
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Test the result of correcting centerpoint and intersection point on 2. border.
(defun F:rb-RESULT-OK (int1 cpt rad segline2 cpt-new int2-new fuzzy )
  (if (and
        int2-new
        ;; distances from center to both intersections are equal?
        (F:rb-EQUAL-DISTANCE cpt-new int1 int2-new (cond
                                                     ((= fuzzy 1) 0.0001) ; for method 1
                                                     ((= fuzzy 2) 0.001) ; for method 2
                                                     ((= fuzzy 3) 0.001) ; for method 3
                                                     ((= fuzzy 4) 0.01) ; for method 4
                                                     ('T 0.0001)
                                                     ))
        (F:rb-ONLINE segline2 int2-new) ; intersection point on 2. border?
        (F:rb-INSIDE-CIRCLE cpt-new cpt rad) ; corrected center inside circle?
        )
    'T                                  ; solution ok
    nil
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 08.08.2005
;;
;; F:rb-CORRECT-CENTERPOINT
;; corrects the position of the calculated circle centerpoint
;; so that the radius distance of `center - 1. border'
;; and `center - 2. border' are the same.
(defun F:rb-CORRECT-CENTERPOINT ( segline1 int1 ballang intdata col /
                                           A3 BIVEC BIVEC1 BIVEC2 CPT CPT-NEW
                                           FLAG INT1->BALLANG INT2 INT2-NEW N
                                           PPT RAD RAD-NEW S2 S4 SEGLINE2 TMP
                                           )

  (setq cpt      (F:rb-DXF "CENTER"  intdata) ; circle centerpoint from intersection data
        rad      (F:rb-DXF "RADIUS"  intdata) ; circle radius from intersection data
        int2     (F:rb-DXF "INT2"    intdata) ; intersection point located on line segment of 2. border
        segline2 (F:rb-DXF "SEGLINE" intdata) ; segment line of 2. border  .........

        ;; point from `int1' with direction of ball angle
        int1->BallAng (polar int1 ballang 1.0)
        )

  ;;============================================================== 1
  (if (setq bivec (F:rb-BISECTING-VECTOR segline1 segline2)) ; `bivec' also used for debug
    (progn
      (if (setq ;; circle center point on bisecting line, line `Ïnt1'-`cpt'
                ;; is perpendicular to segment of 1. border.
                cpt-new (inters (car bivec)(cadr bivec) int1 int1->BallAng nil)
                )
        ;; intersection point on 2. border, line `cpt'-`Ïnt2'
        ;; is perpendicular to segment of 2. border.
        (setq int2-new (F:rb-GET-PERP-PT segline2 cpt-new))
        )
      (if (F:rb-RESULT-OK int1 cpt rad segline2 cpt-new int2-new 1)
        (setq flag 110)                 ; solution found
        (setq bivec nil)                ; for debugging
        )
      )
    )

  ;;============================================================== 2
  (cond
    ((not flag)                         ; no solution found
      (if (car (setq tmp (F:rb-GET-INTERSECTION 120 segline2 cpt rad int1 int1->BallAng int2 nil nil "2")))
        (setq flag     (nth 0 tmp)
              cpt-new  (nth 1 tmp)
              int2-new (nth 2 tmp)
              bivec    (nth 3 tmp)
              )
        )
      )
    )

  ;;============================================================== 3
  (cond
    ((or
       (not flag)                       ; no solution found
       (or
         ;; if `int2-new' is an endpoint of 2. border segment.
         (equal int2-new (car  segline2) 1e-8)
         (equal int2-new (cadr segline2) 1e-8)
         )
       )
      (if (car (setq tmp (F:rb-GET-INTERSECTION 130 segline2 cpt rad int1 int1->BallAng int2 3 3 "3")))
        (setq flag     (nth 0 tmp)
              cpt-new  (nth 1 tmp)
              int2-new (nth 2 tmp)
              bivec    (nth 3 tmp)
              )
        )
      )
    )

  ;;============================================================== 4
  (cond
    ((not flag)
      ;;---------------------------------------------------------- a
      ;; If `int2' is an endpoint of 2. border segment.
      ;;----------------------------------------------------------
      (if (member int2 segline2)
        (progn
          ;; Get the midpoint of line `int1' - `int2'
          (setq bivec1  (F:rb-MIDPT int1 int2)

                ;; Calculate a second point to get a line perpendicular
                ;; to line  `int1' - `int2' to be able to calculate
                ;; the corrected circle center.
                bivec2  (polar bivec1 (- (angle int1 int2) (/ pi 2.0) ) 1.0)

                ;; New corrected centerpoint
                cpt-new (inters bivec1 bivec2 int1 int1->BallAng nil)

                ;; the old intersection point on 2. border will
                ;; be also the new one because it is one of the
                ;; endpoints of the 2. border segment.
                int2-new int2

                n 140                ; value for `flag' if result true
                )

          (if (F:rb-RESULT-OK int1 cpt rad segline2 cpt-new int2-new 4)
            (setq flag  n               ; solution found
                  bivec (list bivec1 bivec2) ; for debug
                  )
            )
          )                             ;_ progn
        ;;-------------------------------------------------------- b
        ;; If `int2' is NOT an endpoint of 2. border segment.
        ;;--------------------------------------------------------
        (progn
          (setq tmp (F:rb-GET-INTERSECTION 141 segline2 cpt rad int1 int1->BallAng int2 nil nil "4b"))

          (if (car tmp)
            (setq flag     (nth 0 tmp)
                  cpt-new  (nth 1 tmp)
                  int2-new (nth 2 tmp)
                  bivec    (nth 3 tmp)  ; for debug
                  )
            ;;---------------------------------------------------- c
            (if (cadr tmp)
              (progn
                (setq cpt-new  (nth 1 tmp)
                      int2-new int2
                      )
                (if (F:rb-RESULT-OK int1 cpt rad segline2 cpt-new int2-new 4)
                  (setq flag 142)       ; solution found
                  )
                )
              )
            ;;----------------------------------------------------
            )                           ;_ if
          )                             ;_ progn
        )                               ;_ if
      )
    )                                   ;_ cond

  ;;==================================================================
  ;;(setq flag nil)

  (cond
    (flag            ; solution found, determine the new circle radius
      (setq rad-new (distance int1 cpt-new))
      )
    ;; IF ALL PREVIOUS METHODS FAILED no perpendicular solution was found ...
    ((not flag)

      ;; ---------------------------------------------------------- ;;
      ;;                                /<- 1. BORDER               ;;
      ;;                               /                            ;;
      ;;                         int1 /                             ;;
      ;;              - - - - - - - -o    +                         ;;
      ;;                            +|\         +                   ;;
      ;;                             | \S3                          ;;
      ;;                         +   |  \          +                ;;
      ;;                             |   \                          ;;
      ;;                 ppt -- + -> o-S4-o cpt     +<- CIRCLE      ;;
      ;;                             |   /                          ;;
      ;;                         + S1|  /          +                ;;
      ;;                             | /S2                          ;;
      ;;                            +|/         +                   ;;
      ;;              - - - - - - - -o    +                         ;;
      ;;                         int2 \                             ;;
      ;;                               \                            ;;
      ;;                                \<- 2. BORDER               ;;
      ;;                                                            ;;
      ;;   S2 & S3 = circle radius                                  ;;
      ;;       cpt = circle center point                            ;;
      ;;       ppt = perpendicular point                            ;;
      ;;      int1 = intersection circle/1. border                  ;;
      ;;      int2 = intersection circle/2. border                  ;;
      ;; ---------------------------------------------------------- ;;

      ;; NON-PERPENDICULAR METHOD:
      ;; Correct the circle center point so that it has the same
      ;; distance to both intersection points.

      (setq ;; Make radius sides `S2' & `S3' equal length,
            ;; side 2 of triangle will also be side 3.
            S2 (/ (+ (distance int1 cpt)(distance cpt int2)) 2.0)

            ;; Get angles of triangle
            tmp (F:rb-SSS (distance int1 int2) ; Side 1 of triangle, S1
                          S2            ; Side 2 of triangle, S2
                          S2            ; Side 3 of triangle, S3
                          )
            )

      ;; Test if it is a triangle and all angles are valid
      (if (F:rb-EVERY '(lambda (ang) (if (< 0 ang pi) 'T nil)) tmp)

        (setq A3 (last tmp)             ; Angle A3 of triangle

              ;; Calculating side `S4' to get the perpendicular
              ;; distance to the corrected centerpoint from line S1.
              ;;
              ;; S1,S2,S3,S4 = sides, A1,A2,A3 = angles
              ;;
              ;;             A1
              ;;             /|\
              ;;         S3 / | \S2
              ;;           / S4  \
              ;;        A2/_ _|_ _\A3
              ;;             S1

              S4 (* (sin A3) S2)        ; Side 4, S4

              ;; ppt = perpendicular pt. from center `cpt' located
              ;;       on LINE `int1 - int2', for getting the angle
              ;;       direction to the new corrected center point.
              ppt (F:rb-GET-PERP-PT (list int1 int2) cpt)

              cpt-new  (polar (F:rb-MIDPT int1 int2) (angle ppt cpt) S4) ; corrected center
              rad-new  S2               ; corrected radius
              int2-new int2 ; corrected intersection point on 2. border
              flag     10               ; also used for debug
              )
        ;; If center and intersection points are colinear.
        (if (> (distance int1 int2) 0)
          (setq cpt-new  (F:rb-MIDPT int1 int2) ; corrected center
                rad-new  (distance int1 cpt-new) ; corrected radius
                int2-new int2
                flag     1              ; also used for debug
                )
          (setq int2-new nil)
          )
        )

      )
    )

  ;;--------------------------------------------------------------
  ;; Test if the current intersection point `int2-new' on
  ;; 2. border is in front of the last one or equal (corner point)
  ;;--------------------------------------------------------------
  (cond
    ;; If the very first corrected intersection point `int2-new' is calculated
    ((not **DIR1*)

      ;; set intersection point direction numbers for the first run to 0
      (setq **DIR1* 0
            **DIR2* 0
            )
      )
    ;; If the second corrected intersection point `int2-new' is calculated
    ;; or `**DIR1*' is still 0.
    ((= **DIR1* 0)

      ;; Save direction number of second corrected intersection point,
      ;; it will be used for comparison with the 3. intersection point.
      ;; The first value for comparison must be -1 for right or
      ;; 1 for left side from line `**OLD-INT1*' - `**OLD-INT2-NEW*'.
      (setq **DIR1* (F:rb-WHAT-SIDE    ;  value for `int2-new' compare
                     **OLD-INT1*
                     **OLD-INT2-NEW*
                     int2-new
                     )
            )
      )
    ('T          ; for all following corrected intersection points ...
      (if (and int2-new **OLD-INT2-NEW*)
        ;; Calculate direction number of current corrected intersection point
        ;; it should be -1 for right or 1 for left side, NOT 0.
        (setq **DIR2* (F:rb-WHAT-SIDE  ;  value for `int2-new' compare
                       **OLD-INT1*
                       **OLD-INT2-NEW*
                       int2-new
                       )
              )
        (setq **DIR2* nil) ; nil = intersection on 2. border point is not ok
        )                               ;_ if
      )                                 ;_ 'T
    )
  ;;------------------------------------------------------------------>>
  (if (and
        (or
          ;; `int2-new' is correct if `**DIR2*' and `**DIR1*'
          ;; have always the same values, -1 or 1.
          (= **DIR2* **DIR1*)

          (= **DIR2* 0) ; if `**DIR2*' = 0 it's the first run or a corner point, ok.
          )
        (not (equal cpt-new **OLD-CPT-NEW* 1e-8)) ; avoid double center points
        )
    (progn
      (F:rb-DEBUG 1 (list segline1 segline2 bivec int1 cpt rad cpt-new int2-new flag col) ) ; for debugging

      (setq ;; save current corrected center & intersection point on 1. border,
            ;; they will be used in the next run to decide whether the
            ;; next corrected center point is going backward or forward.
            **OLD-INT1*     int1
            **OLD-CPT-NEW*  cpt-new
            **OLD-INT2-NEW* int2-new
            )

      ;; for error checking
      (if (member flag '(1 10))
        ;; count not perpendicular center points
        (setq **NOT-PERP-CPTS* (1+ **NOT-PERP-CPTS*))
        )

      ;; RETURN corrected centerpoint, corrected radius & corrected
      ;; intersection pt. on 2. border and flag for error checking
      (list cpt-new rad-new int2-new flag)
      )
    (progn
      ;;(break (strcat (itoa **DIR2*) " " (itoa **DIR1*)))
      (F:rb-DEBUG 3 (list int1 cpt int2 segline1 segline2) )

      ;; count pts. that could not be corrected
      (setq **CORRECTION-ERRORS* (1+ **CORRECTION-ERRORS*))

      ;; RETURN nil if `int2-new' is on the wrong side.
      nil
      )
    )
  ;;------------------------------------------------------------------<<
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; T if 'pred is non-nil for every element
;; or nil if some elements are nil when applied to pred
;; (apply 'and (mapcar 'pred lst))
;;
;; one-arg optimized alisp version
;;   don't process the whole list,
;;   break at the first nil
(defun F:rb-EVERY (pred lst / res)
  (setq	res (apply pred (list (car lst)))
        lst (cdr lst)
        )
  (while (and res lst)
    (setq res (apply pred (list (car lst)))
          lst (cdr lst)
          )
    )
  res
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; This function returns the point in a list that is closest to a
;; supplied point. BASE must be a point and LST must be a list of points.
(defun F:rb-CLOSEST-PT (base lst / CP tmp)
  (terpri)
  (princ "\n   searching closest point ...\r")
  (foreach X lst
           ;;...........................
           (setq tmp (F:rb-SPIN tmp))
           (princ (strcat "\r " tmp ))
           ;;...........................
           (if CP
             (if (< (distance base X) (distance base CP))
               (setq CP X)
               )
             (setq CP X)
             )
           )
  (F:rb-CLEAR-CMDLINE 50)
  CP                              ; return closest point or point pair
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Conversion pixel to drawing units
(defun F:rb-PIX2UNITS (pix)
  (* pix (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE")))))
;;;
;;;-------------------------------------------------------------------
;;;
;; check point list for self intersecting segments
(defun F:rb-PL-ISECT ( lst / A B C D fuzzy INT J LEN1 LST1 LST2 LST3 N TMP )
  (setq fuzzy 1e-6)
  ;; --- --- --- --- --- ---
  (if (not (listp (caar lst)))
    (progn
      (foreach x lst
               (if (and a x)
                 (setq lst1 (cons (list a x) lst1))
                 )
               (setq a x )
               )
      (setq lst1 (reverse lst1) )
      )
    (setq lst1 lst)
    )
  ;; --- --- --- --- --- ---

  (setq n 0
        len1 (length lst1)
        )

  (while (and lst1
              (not int)
              )

    (setq tmp (car lst1)
          a   (car  tmp)
          b   (cadr tmp)
          )

    ;;------------------------------------
    ;; test current segment with the next
    ;; 20 segments for intersection.
    (setq j 1)
    (while (and (< j len1) (< j 21)
                (not int)
                )
      (setq tmp (nth j lst1)
            c   (car  tmp)
            d   (cadr tmp)
            )
      (if (and (not (equal b c fuzzy))
               (not (equal a d fuzzy))
               )
        (setq int (inters a b c d 'T))
        )
      (setq j (1+ j))
      )
    ;;------------------------------------

    (if (not int)
      (setq lst2 (cons (list a b) lst2)
            lst1 (cdr lst1)
            )
      (setq lst1 (cdr lst1))
      )
    (setq len1 (- len1 1))
    )
  (setq lst3 (append (reverse lst2) lst1))
  (setq tmp (list (caar lst3)))
  (foreach x lst3
           (setq tmp (cons (nth 1 x) tmp))
           )

  ;; return intersection point and list without
  ;; intersecting segments on which point was found.
  (list int tmp)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Check circle center point position by checking the
;; lines `center - hit_point_on_2. border' for intersection.
(defun F:rb-VERIFY-CENTER-POS ( lst / A B C D INT INTLST fuzzy
                                    J LEN1 LST1 N RAD TMP
                                    )

  (setq fuzzy 1e-16)
  ;;------------------------------------------------
  (if (not (listp (caar lst)))
    (progn
      (foreach x lst
               (if (and a x)
                 (setq lst1 (cons (list a x) lst1))
                 )
               (setq a x )
               )
      (setq lst1 (reverse lst1) )
      )
    (setq lst1 lst)
    )
  ;;------------------------------------------------
  (setq n 0
        len1 (length lst1)
        )

  (while (and lst1
              (not int)
              )

    (setq tmp (car lst1)
          a   (car  tmp)                ; center point
          b   (cadr tmp)                ; hit point on 2. border
          )

    ;;------------------------------------
    ;; test current radius line with the
    ;; next 10 radius lines for intersection.
    (setq j 1)
    (while (and (< j len1) (< j 10) )
      (setq tmp (nth j lst1)
            c   (car  tmp)              ; center point
            d   (cadr tmp)              ; hit point on 2. border
            )

      ;; `a' & `c' = segment pts. on centerline, `cpt' & `int2',
      ;;             one of them can have a wrong position.
      ;; `b' & `d' = segment pts. on 2. border, `cpt' & `int2'.
      (if (not (equal b d fuzzy))
        (setq int (inters a b c d 'T))
        )

      (if (or
            ;; if lines intersect themselves
            int
            ;; if lines are identically
            (and (equal a c fuzzy)(equal b d fuzzy))
            )
        (setq intlst (F:rb-ADJOIN (list a b) intlst)
              intlst (F:rb-ADJOIN (list c d) intlst)
              )
        )

      (setq j (1+ j))
      )
    ;;------------------------------------
    (setq lst1 (cdr lst1))
    (setq len1 (- len1 1))
    )

  ;; return intersection point list or nil
  (if intlst
    intlst
    nil
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-REMOVE-INTERS (lst / int intlst lst1 n tmp)
  (setq int (car lst)
        lst1 (last lst)
        n 0
        )
  (princ "\r     working...\r")
  (while int
    (setq intlst (cons int intlst))
    (F:rb-GRDRAW-POINT int 10 "X")
    (princ (strcat "\r  " (itoa (setq n (1+ n))) "\r"))
    (setq tmp (F:rb-PL-ISECT lst1)
          int (car  tmp)
          lst1 (last tmp)
          )
    )
  (list lst1 intlst)
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-MARK-INTERS (intlst lay col1 / tmp)
  (princ "\n Mark each intersection point with a RED circle... \n")
  (F:rb-SET-LAYER lay col1 'T)          ; make mark layer
  (setq tmp (getvar "CECOLOR"))
  (setvar "CECOLOR" "1")

  ;; draw circle on each intersection point
  (foreach int intlst
           (command "_circle" int (F:rb-PIX2UNITS (getvar "APERTURE")))
           )

  (setvar "CECOLOR" tmp)
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-ADJOIN ( ele lst / tmp )
  (if (= (type lst) 'SYM)
    (setq tmp lst
          lst (eval tmp)
          )
    )
  (setq lst (cond ((member ele lst) lst)
                  ('T (cons ele lst)))
        )
  (if tmp (set tmp lst) lst)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; looking for self intersecting segments of centerline
(defun F:rb-VERIFY-CENTERLINE ( centerline lay col1 / centerline-weeded flag int intlst tmp)

  (princ "\n.")
  (princ "\r Verifying Centerline for self intersecting segments ...\r")
  (princ)

  ;; Checks if centerline segments intersect on themselves,
  ;; returns the first intersection point and the list
  ;; without the segments on which the intersection was found.
  (setq int (F:rb-PL-ISECT centerline))

  (cond
    ;; centerline OK
    ((not (car int))
      (princ "\r Verifying Centerline for self intersecting segments ... OK\r")
      (setq flag 0)
      )
    ;; centerline NOT ok
    ('T
      (F:rb-GRDRAW-POINT (car int) 2 "X") ; mark first intersection

      (princ "\n  *** CAUTION - CENTERLINE SEGMENTS INTERSECT ON THEMSELVES.")
      (princ "\n  ............. A change of increment along 1. border or a change")
      (princ "\n  ............. of the ball increase factor might avoid this effect.")

      (if (= "Y" (F:rb-UKWORD
                  1
                  "N Y"
                  "\n Do you want to remove the self intersecting line segments? [Yes/No] "
                  nil
                  )
             )
        (progn
          (princ "\n Removing line segments on which intersection points are found... \n")

          ;; create centerline list without intersecting line
          ;; segments and list with intersection points
          (setq tmp (F:rb-REMOVE-INTERS int)
                centerline-weeded (car tmp)
                intlst (cadr tmp)
                )

          ;; mark all segment self intersection points with a circle
          (F:rb-MARK-INTERS intlst lay col1)
          (princ "\r ...DONE, all intersections are marked with a GREEN circle")
          (setq flag 1)
          )
        (progn
          (if (= "Y" (F:rb-UKWORD
                      1
                      "N Y"
                      "\n Do you want to draw self INTERSECTING centerline ? [Yes/No] "
                      nil
                      )
                 )
            (progn
              ;; search all centerline segment self intersections
              (setq tmp (F:rb-REMOVE-INTERS int)
                    intlst (cadr tmp)
                    )

              ;; mark all segment self intersections
              (F:rb-MARK-INTERS intlst lay col1)

              (setq flag 2)
              )
            (setq flag 3)               ; do nothing
            )
          )
        )
      )
    )
  (cond
    ((= flag 0) (list 0 centerline)        ) ; return original centerline
    ((= flag 1) (list 1 centerline-weeded) ) ; return centerline without intersecting segments
    ((= flag 2) (list 2 centerline)        ) ; return uncorrected centerline (original)
    ((= flag 3) (list 3 nil)               ) ; return no centerline
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 09.12.2005
;;
;; Function to loop thru a set of lines looking for an intersection,
;; stops when the first intersection is found.
(defun F:rb-SEARCH-INTERS (cirent int1 ballang1 boundary / cpt fnd
                                  subl-index seg-index
                                  rad tmp subl-len subl seg1 seg2
                                  )

  (if cirent
    (progn
      (cond
        ;; If `cirent' is a list
        ((listp cirent)
          (setq cpt (car  cirent)
                rad (cadr cirent)
                )
          )
        ;; If `cirent' is an entity
        ('T
          (setq tmp (entget cirent)
                cpt (F:rb-DXF 10 tmp)
                rad (F:rb-DXF 40 tmp)
                )
          )
        )

      (cond
        ((= boundary 1)            ; 1 = first boundary, list **ALIST*
          ;; Move centerpoint from base segment to avoid
          ;; intersection of circle with base segment.
          (setq cpt (polar cpt ballang1 1e-8) )

          (setq seg-index 0)           ; start with first line segment
          (while (< seg-index (getvar "USERI1")) ; "USERI1" = length of list **ALIST*
            ;;(grdraw (car(nth seg-index **ALIST*))(cadr(nth seg-index **ALIST*)) seg-index)
            (if (setq fnd (F:rb-INTERS-LC-1
                           (nth seg-index **ALIST*)
                           int1 cpt rad boundary
                           ))
              ;; If intersection is found set segment index to
              ;; maximum value to stop the loop.
              (setq seg-index (1+ (getvar "USERI1")))

              ;; No intersection found, take the next segment
              (setq seg-index (1+ seg-index))
              )
            )
          )

        ((= boundary 2)     ; 2 = second boundary, list **BLIST-SPLIT*
          (if (> (getvar "USERI4") 0)
            (progn
              ;; Start with the last used sublist.
              ;; "USERI4" = index number of last used sublist,
              ;; "USERI3" = 0 or -1, used to lower sublist index number when during
              ;;            the last run no intersection on 2. boundary was found
              ;;            to make sure not to miss an intersection point.
              (setq subl-index (+ (getvar "USERI4")(getvar "USERI3"))) ; index of sublist in **BLIST-SPLIT*

              ;; Reset lowering value of sublist index
              (setvar "USERI3" 0)
              )
            (setq subl-index 0) ; start with first sublist of list **BLIST-SPLIT*
            )

          ;;...............................................1
          (while (< subl-index (getvar "USERI2")) ; "USERI2" = list length of **BLIST-SPLIT*

            ;; **BLIST-SPLIT* = `Blist' split into
            ;; sublists each containing 20 segments to
            ;; minimize calculations for intersections.
            (setq subl (nth subl-index **BLIST-SPLIT*) ; subl = sublist with 20 segments

                  subl-len (length subl) ; length of sublist (20 segments)
                  seg-index 0   ; start with first segment in sublist.
                  )

            ;;---------------------------------------------2
            (while (and (< seg-index subl-len) (< subl-index (getvar "USERI2")))
              (if (setq fnd (F:rb-INTERS-LC-1
                             (nth seg-index subl)
                             int1 cpt rad boundary
                             ))
                ;; If found
                (progn
                  (setvar "USERI4" subl-index) ; save sublist index where the segment was found
                  (setq subl-index (1+ (getvar "USERI2"))) ; stop loop, intersection point found
                  )

                ;; If not found continue with the next segment in sublist
                (setq seg-index (1+ seg-index)) ; index of segment in sublist
                )
              )                         ;_ while 2, segments
            ;;---------------------------------------------2

            ;; If not found continue with the next sublist of segments
            (setq subl-index (1+ subl-index) ) ; index of sublist in **BLIST-SPLIT*
            )                       ;_ while 1, sublists with segments
          ;;...............................................1

          ;; If last sublist was processed, reset
          ;; index number of last used sublist to the
          ;; first sublist of `**BLIST-SPLIT*'.
          (if (>= subl-index (getvar "USERI2")) (setvar "USERI4" 0) )

          )
        )                               ;_ cond
      )                                 ;_ progn
    (setq fnd nil)
    )                                   ;_ if
  fnd
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 24.11.2005
;;
;; If a line segment is close to the circle center
;; than test whether it is intersecting the circle.
;;
(defun F:rb-INTERS-LC-1 (line int1 cpt rad boundary)
  (if (< (distance (car line) cpt)
         (+ rad rad (apply 'distance line))
         )
    (if (and int1 (F:rb-ONLINE line int1))
      ;; return if segments are colinear
      (list
       (cons "CENTER"   cpt)
       (cons "RADIUS"     0)
       (cons "INT2"    int1)
       (cons "SEGLINE" line)
       (cons "DIST2"      0)
       )
      (F:rb-INTERS-LC line cpt rad boundary)
      )
    nil
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 05.08.2005
;;
;; Find intersection of Line and Circle radius.
;; LINE = 2 points, CPT = circle centerpoint, RAD = circle radius
(defun F:rb-INTERS-LC ( line cpt rad boundary / d1 d2 int2 ppL)

  ;; Point on `line' perpendicular to circle center `cpt'
  (setq ppL (F:rb-GET-PERP-PT line cpt) )

  (cond
    ((>= (- rad (distance cpt ppL)) 0.0)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
      ;;                       +                      ;;
      ;; LINE pt. 1      +     |     +     LINE pt. 2 ;;
      ;;    O------------------O<- ppL/int2 -----O    ;;
      ;;              +        |        +             ;;
      ;;                       |                      ;;
      ;;             +         O<- cpt   +            ;;
      ;;                       |                      ;;
      ;;     CIRCLE ->+        |<- rad  +             ;;
      ;;                       |                      ;;
      ;;                 +     |     +                ;;
      ;;                       +                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

      (setq d1   (distance cpt (car  line))
            d2   (distance cpt (cadr line))
            int2 (cond      ; `int2' = intersection point on 2. border
                   ;;------------------------------------------------- 1
                   ((and
                      (= boundary 1)    ; test only with 1. boundary
                      ;; Is Circle center `cpt' situated on line `line'?
                      (equal (+ d1 d2) (apply 'distance line) 1e-8 )
                      )
                     ppL ; if circle center `cpt' and `line' points are colinear
                     )
                   ;;------------------------------------------------- 2
                   ;; 1. LINE point OUTSIDE circle
                   ;; 2. LINE point OUTSIDE circle
                   ((and (> d1 rad) (> d2 rad))
                     (if (inters (car line)(cadr line) cpt ppL 'T)
                       ppL
                       nil
                       )
                     )
                   ;;------------------------------------------------- 3
                   ;; 1. LINE point INSIDE or ON            circle
                   ;; 2. LINE point INSIDE or OUTSIDE or ON circle
                   ((<= d1 rad)
                     (if (inters (car line)(cadr line) cpt ppL 'T)
                       ppL
                       (car line)
                       )
                     )
                   ;;------------------------------------------------- 4
                   ;; 1. LINE point INSIDE or OUTSIDE or ON circle
                   ;; 2. LINE point INSIDE or ON circle
                   ((<= d2 rad)
                     (if (inters (car line)(cadr line) cpt ppL 'T)
                       ppL
                       (cadr line)
                       )
                     )
                   ;;------------------------------------------------- 'T
                   ('T nil )
                   )                    ;_ cond
            )                           ;_ setq `int2'
      )
    )                                   ;_ cond

  ;;(F:rb-DEBUG 4 (list cpt rad int2 line ppL) )

  (if int2
    (list
     (cons "CENTER" cpt)
     (cons "RADIUS" rad)
     (cons "INT2" int2)
     (cons "SEGLINE" line)
     (cons "DIST2" (distance cpt int2))
     )                                  ; return list
    nil                                 ; return nil
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; (F:rb-SSS (getdist "Side A, ") (getdist "Side B, ")(getdist "Side C: "))
;; Returns 3 angles in radians, side(A), side(B) and side(C) are known.
(defun F:rb-SSS (side_A side_B side_C / #ACOS #SQ)

  ;; A,B,C = sides, A1,A2,A3 = angles
  ;;
  ;;              A1
  ;;             / \
  ;;     Side C /   \ B
  ;;           /     \
  ;;        A2/_ _ _ _\A3
  ;;              A

  (defun #SQ (n) (expt n 2.0))

  ;; Returns inverse cosinus in radians
  (defun #ACOS (cosi)
    (cond
      ((> (abs cosi) 1.0) nil)        ; if ARC-COSINE ERROR return nil
      ((equal cosi  1.0 1e-16) 0.0 )
      ((equal cosi -1.0 1e-16)  pi )
      ('T (- (/ pi 2) (atan (/ cosi (sqrt (- 1 (* cosi cosi))))) ) )
      )
    )

  ;; return list with angles A1, A2 and A3.
  (if (and (> side_A 0)(> side_B 0)(> side_C 0))
    (mapcar '#ACOS                      ; calculate angles in radian
            (list
             ;; cosine side A
             (/ (- (+ (#SQ side_B) (#SQ side_C)) (#SQ side_A)) (* 2 side_B side_C))
             ;; cosine side B
             (/ (- (+ (#SQ side_A) (#SQ side_C)) (#SQ side_B)) (* 2 side_A side_C))
             ;; cosine side C
             (/ (- (+ (#SQ side_A) (#SQ side_B)) (#SQ side_C)) (* 2 side_A side_B))
             )
            )
    (list 0 0 0)
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; (F:rb-GET-PERP-PT (list (getpoint)(getpoint))(getpoint))
;; Get a point perpendicular to `pt' on `vector'.
(defun F:rb-GET-PERP-PT (vector pt / a #SQ)

  (defun #SQ (n) (expt n 2.0))

  (if (equal (car vector) (cadr vector) 0)
    (car vector)
    ;; else
    (polar (cadr vector)
           (angle (cadr vector)(car vector))

           ;; calculate distance
           ;;-----------------------------------
           (/ (- (+
                  (#SQ (setq a (distance (car  vector) (cadr vector)))) ; side a
                  (#SQ (distance (cadr vector) pt)) ; side b
                  )                     ;_ +
                 (#SQ (distance (car  vector) pt)) ; side c
                 )                      ;_ -

              (* 2.0 a)                 ; side a * 2
              )                         ;_ /
           ;;-----------------------------------
           )                            ;_ polar
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Create a line perpendicular to line `p1' - `p2'
(defun F:rb-MAKE-PERP-LINE (p1 p2 dist)
  (if (<= dist 0)(setq dist 1.0))
  (list
   (polar p1 (- (angle p2 p1) (/ pi 2.0) ) dist ) ; 1.5708 = pi / 2
   (polar p1 (+ (angle p2 p1) (/ pi 2.0) ) dist )
   )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Returns midpoint between 2 points
(defun F:rb-MIDPT (pt1 pt2)
  (list (/ (+ (car   pt1) (car   pt2)) 2.0) ; X
        (/ (+ (cadr  pt1) (cadr  pt2)) 2.0) ; Y
        (/ (+ (caddr pt1) (caddr pt2)) 2.0) ; Z
        )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Test counter-clockwise of 3 points (the rotation),
;; twice the signed area of a simple 2d triangle, Z ignored.
;; (F:rb-WHAT-SIDE (getpoint)(getpoint)(getpoint))
(defun F:rb-WHAT-SIDE (a b c / tmp)     ; `c' = point to test
  (setq tmp (- (* (- (car  b)(car  a))
                  (- (cadr c)(cadr a))
                  )
               (* (- (car  c)(car  a))
                  (- (cadr b)(cadr a))
                  )
               ))
  (cond
    ((< tmp 0) -1)
    ((> tmp 0)  1)
    ('T 0)
    )    ; return 1 for LEFT side, -1 for RIGHT side or 0 for ON line.
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 10.12.2005
;;
;; Find rotation of polyline,
;; return: -1 = counterclockwise, 1 = clockwise, 0 = cannot be determined
(defun F:rb-POLY-ROT-CCW (vlist / area n p1 p2)
  (setq area 0
        n    0
        )

  (repeat (1- (length vlist))
          (setq p1 (nth n vlist)
                p2 (nth (1+ n) vlist)
                area (+ area
                        (-  (* (cadr p1) (car p2))
                            (* (cadr p2) (car p1))
                            )
                        )
                n (1+ n)
                )
          )

  (setq p1 (last vlist)
        p2 (car  vlist)
        area (+ area
                (-  (* (cadr p1) (car p2))
                    (* (cadr p2) (car p1))
                    )
                )
        ;;area (/ area 2.0)
        )

  ;;(print area)

  (cond
    ((< area 0.0) -1)
    ((> area 0.0)  1)
    ('T 0)
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 22.11.2005
;; Mark a point
(defun F:rb-GRDRAW-POINT (pt color mode / dd num inc ang anginc side )

  (setq dd (F:rb-PIX2UNITS (getvar "APERTURE"))) ; drawing distance
  (setq pt (trans pt 2 1))              ; Trans 1st pt from DCS to UCS

  (cond
    ;;----------------
    ;; CROSS / X
    ;;----------------
    ((member (strcase mode) '("CROSS" "CR" "X"))
      (if (= (strcase mode) "X")
        (setq ang 0.785398163)          ; Set to draw X
        (setq ang 0)                    ; Set to draw cross
        )
      (setq anginc 1.570796327
            inc    dd
            )
      (repeat 2                         ; Draw X or cross
              (grdraw
               (polar pt ang inc)
               (polar pt (+ ang pi) inc)
               color
               )
              (setq ang (+ ang  1.570796327)) ; Rotate ANG 90 degree
              )
      )
    ;;----------------
    ;; BOX / CIRCLE
    ;;----------------
    ((member (strcase mode) '("BOX" "B" "CIRCLE" "CI"))
      (setq inc (* 2 dd))
      (if (wcmatch (strcase mode) "B*")
        ;; Set variables for square
        (setq num    4                  ;  4 sides
              anginc 1.570796327        ; 90 degree
              ang    0                  ;  0 degree
              side   inc                ; Each side INC long
              pt     (polar pt -0.785398164 (* inc 0.707106781)) ; Start
              )
        ;; Set variables for circle
        (setq num    12         ; 12 sides at angular inc of 30 degree
              anginc 0.523598776        ; 30 degree
              ang    1.308996939 ; 75 degree (90 degree minus 1/2 ANGINC)
              side   (* inc (sin 0.261799388)) ; Each side INC x sin 15 degree long
              pt     (polar pt 0 (* 0.5 inc)) ; Start
              )
        )
      (repeat num                       ; 4 for square, 12 for circle
              (grdraw pt
                      (setq pt (polar pt (setq ang (+ ang anginc)) side))
                      color
                      )
              )
      )
    ;;----------------
    ;; VERTICAL TICK
    ;;----------------
    ((member (strcase mode) '("TICK" "T"))
      (setq inc (* 2 dd))
      (grdraw pt (polar pt 1.570796327 inc) color)
      )
    ;;----------------
    ;; JUST DRAW POINT
    ;;----------------
    ('T
      (grdraw pt pt color)
      )
    )
  (princ)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Zoom factor or zoom position changed?
(defun F:rb-VIEWCHANGED?	(oldviewparam)
  (not
   (or (equal (getvar "VIEWSIZE") (car oldviewparam))
       (equal (getvar "VIEWCTR") (last oldviewparam))
       )
   )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Gets view parameter (viewsize viewctr)
(defun F:rb-GETVIEWPARAM () (list (getvar "VIEWSIZE") (getvar "VIEWCTR")))
;;;
;;;-------------------------------------------------------------------
;;;
;; Determine the length of an arrow drawn by F:rb-GRDRAW-ARROW
(defun F:rb-ARROW-LEN () (/ (car (F:rb-GETVIEWPARAM)) 9.0) )
;;;
;;;-------------------------------------------------------------------
;;;
;; (F:rb-GRDRAW-ARROW (getpoint)(getangle) (F:rb-ARROW-LEN) 1)
;; Draw an arrow with Grdraw
;; Arguments:
;; Startpt-	arrow start point
;;		ang - angle
;;		len - length of arrow
;;		col - color
(defun F:rb-GRDRAW-ARROW (p1 ang len col / p2 p3 p4)
  (setq	p2 (polar p1 ang len)
        p3 (polar p2 (+ ang 2.75) (* len 0.35))
        p4 (polar p2 (- ang 2.75) (* len 0.35))
        )
  (grvecs (cons col (list p1 p2 p2 p3 p2 p4)) )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 29.11.2005
;;
;; Get offset direction
;; Arguments:	2 points 1 prompt
;; Return :	list with side point,
;;          direction number (1 for LEFT or -1 for RIGHT),
;;          angle of arrow.
;;
(defun F:rb-GET-SIDE (pt1 pt2 pt3 pt4 prmpt / ang arrowleft arrowright
                          grval direction tmplen viewparam
                          oldviewparam olddirection arrow
                          )

  (setq ang	       (angle pt1 pt2)
        arrowleft  (list (trans pt1 0 1) (+ ang (* 0.5 pi)))
        arrowright (list (trans pt1 0 1) (- ang (* 0.5 pi)))
        )
  (princ (strcat "\n " prmpt))
  (while (and
           (/= grval 13)                ; <Return> key
           (/= 3 (car (setq grval (grread t)))) ; mouse
           )
    (setq grval (cadr grval) )
    (cond
      ;;--------------------------------
      ;; Grread evaluation if point list
      ;;--------------------------------
      ((listp grval)
        ;; calculate values of actual cursor position
        (setq viewparam (F:rb-GETVIEWPARAM)
              tmplen    (/ (car viewparam) 7.5)
              direction (F:rb-WHAT-SIDE (trans pt1 1 0) (trans pt2 1 0) (trans grval 1 0))
              )
        ;; Redraw screen, if necessary
        (if (or (F:rb-VIEWCHANGED? oldviewparam)
                (and (/= direction olddirection) (/= direction 0))
                )
          (progn
            (cond
              ((= direction  1) (setq arrow arrowleft)) ; left side
              ((= direction -1) (setq arrow arrowright)) ; right side
              )

            ;; draw arrow
            (redraw)
            (F:rb-GRDRAW-ARROW (car arrow) (cadr arrow) tmplen 4)

            ;; mark the start and direction of the 1. border
            (F:rb-MARK-POINT pt1 pt2 1)

            ;; mark the start and direction of the 2. border
            (if (and pt3 pt4) (F:rb-MARK-POINT pt3 pt4 2))

            )
          )
        ;; save actual values
        (setq olddirection direction
              oldviewparam viewparam
              )
        )
      ;;--------------------------------
      ;; If new value, redraw screen
      ;;--------------------------------
      ('T (redraw) (setq oldviewparam nil))
      )
    )
  ;; return endpoint of arrow (side point), arrow angle and direction
  (list (car arrow) (cadr arrow) (if (= direction 1) '("LEFT" 1) '("RIGHT" -1)))
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-MARK-POINT (p1 p2 col / p3)
  (F:rb-GRDRAW-POINT p1 9 "X")
  (F:rb-GRDRAW-ARROW (setq p3 (polar
                               p1
                               (angle p2 p1)
                               (F:rb-ARROW-LEN)
                               )
                           )
                     (angle p1 p2)
                     (* 0.75 (F:rb-ARROW-LEN)) ; arrow length
                     col                ; arrow color
                     )
  p3
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Prints a pinwheel on the command line
(defun F:rb-SPIN (ch)
  (cond
    ((= ch "-" ) "\\")
    ((= ch "\\") "|" )
    ((= ch "|" ) "/" )
    ('T "-")
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 25.09.2005
;;
(defun F:rb-GET-TIME () (* 86400 (getvar "TDUSRTIMER")) ) ; seconds
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 28.07.2005
;;
;; Save start time
(defun F:rb-START-TIMER (msg)
  (setq **S_TIME* (getvar "TDUSRTIMER") )
  (if (= (type msg) 'STR)
    (princ
     (strcat (if (/= msg "") (strcat msg ": ") "")
             (F:rb-PARSE-TIME (getvar "CDATE"))
             "\n"
             )
     )
    )
  (princ)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 31.07.2005
;;
(defun F:rb-PARSE-TIME (cdate / date_str day hrs mins month secs year)
  (setq date_str (rtos cdate 2 8)
        year     (substr date_str  3 2)
        month    (substr date_str  5 2)
        day      (substr date_str  7 2)
        hrs      (substr date_str 10 2)
        mins     (substr date_str 12 2)
        secs     (substr date_str 14 2)
        )
  (strcat                               ; return
   ;;day "." month "." year " / "
   hrs ":" mins ":" secs
   )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 24.11.2005
;;
(defun F:rb-STOP-TIMER (msg format / days e_time hrs mns secs msecs
                            ;; local functions
                            #STRINGP #STRING-NOT-EMPTYP
                            )

  ;; #STRINGP  - string predicate: is `s' a string?
  (defun #STRINGP (s) (= (type s) 'STR))

  ;; #STRING-NOT-EMPTYP  - is `s' a not empty string?
  (defun #STRING-NOT-EMPTYP (s) (and (#STRINGP s) (/= s "")))

  (setq e_time (- (getvar "TDUSRTIMER") **S_TIME*)
        days   (fix e_time)             ; number of days
        e_time (- e_time (float days))
        hrs    (fix (/ e_time (/ 1.0 24) )) ; number of hours
        e_time (- e_time (float (* hrs (/ 1.0 24))))
        mns    (fix (/ e_time (/ 1.0 24 60))) ; number of minutes
        e_time (- e_time (float (* mns (/ 1.0 24 60))))
        secs   (fix (/ e_time (/ 1.0 24 60 60))) ; number of seconds
        e_time (- e_time (float (* secs (/ 1.0 24 60 60))))
        msecs  (fix (/ e_time (/ 1.0 24 60 60 1000))) ; number of milli seconds
        )

  (cond
    ((= format 1)      ; Format: 00:00:00.000 (seconds + milliseconds)
      (princ
       (strcat (if (#STRING-NOT-EMPTYP msg) (strcat msg ": ") "")
               (if (> days 0.0) (strcat (itoa days) " day(s) and ") "")
               (if (= (strlen (itoa hrs)) 1) (strcat "0" (itoa hrs) ":") (strcat (itoa hrs) ":"))
               (if (= (strlen (itoa mns)) 1) (strcat "0" (itoa mns) ":") (strcat (itoa mns) ":"))
               (itoa secs) "." (itoa msecs) " hrs. \n"
               )
       )
      )
    ('T          ; Format: days hours minutes seconds (+ milliseconds)
      (princ
       (strcat (if (#STRING-NOT-EMPTYP msg) (strcat msg ": ") "")
               (if (> days 0)
                 (strcat (itoa days) " " (F:rb-GRAMMAR "day" days) " ")
                 ""
                 )
               (if (> hrs  0)
                 (strcat (itoa hrs) " " (F:rb-GRAMMAR "hr" hrs) ". ")
                 ""
                 )
               (if (> mns  0)
                 (strcat (itoa mns) " " (F:rb-GRAMMAR "min" mns) ". ")
                 ""
                 )
               (itoa secs) "." (itoa msecs) (if (<= secs 1) " sec.\n" " secs.\n" )
               )
       )
      )
    )
  (princ)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; update the centerpoint & radius of a circle
;; (F:rb-ENTMOD-CIRCLE <entity> <centerpoint> <radius>)
(defun F:rb-ENTMOD-CIRCLE (cent cpt rad / tmp)
  (setq tmp (entget cent)
        tmp (subst (cons 10 cpt)(assoc 10 tmp) tmp) ; center
        tmp (subst (cons 40 rad)(assoc 40 tmp) tmp) ; radius
        )
  (entmod tmp)
  (princ)          ; use `(princ)' to force a screen refresh on circle
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; I:EntSelF
;;
;; Purpose
;; A cross between (entsel) & (ssget). Unlike (entsel), you may provide
;; a filter. Unlike (ssget), you may provide a prompt.
;;
;; Arguments
;; Msg, string for selection prompt, or nil. Filter, (ssget)-style filter, or nil.
;;
;; Example
;; (I:EntSelF "\nSelect polyline: " '((0 . "Polyline,LWPolyline")))
;;
;; Notes
;; The above example would return this, if a pline was within the
;; pickbox area: (<Objectname: 4rb0e20> (106.803 164.815 0.0)), or nil if no polyline.
;; This routine is useful when you have congested drawings that (entsel) would
;; normally have fits with. The advantage in this case, is that this
;; routine acts like an (entsel), but really uses an (ssget) with a crossing
;; box the size of your pickbox.
;; This permits the use of selection set filters, to give you control over what
;; the user selects.
;; Author
;; R. Robert Bell
;;
(defun F:rb-EntSelF (Msg                ; selection prompt
                     Filter             ; filter list
                     / EntN             ; (entsel) list
                     pbDist            ; pickbox size in drawing units
                     PtPick         ; point of selection from (entsel)
                     ssPick)            ; selection set
  (setvar "ERRNO" 0)                    ; clear ErrNo for loop
  (while (and (not (setq EntN (if Msg   ; if selection prompt
                                (entsel Msg) ; then (entsel) w/prompt
                                (entsel)
                                )       ;_ if
                         )              ;_ setq
                   )                 ; while no selection (or no exit)
              (/= 52 (getvar "ERRNO"))
              )                         ;_ and
    )                                   ; if null response
  (cond (EntN                           ; if not exit
          (setq pbDist (abs  ; return absolute number, get pixel ratio
                        (/ (* (/ (getvar "PICKBOX") (cadr (getvar "SCREENSIZE")))
                              (getvar "VIEWSIZE")
                              )         ; apply to viewsize (in units)
                           (sin (* 0.25 pi))
                           )            ;_ /
                        )               ; at 45∞
                PtPick (cadr EntN)
                )                       ; get point of pick
          (if (setq ssPick (ssget "_C"  ; if entities in crossing
                                  (polar PtPick (* 1.25 pi) pbDist)
                                        ; lower left
                                  (polar PtPick (* 0.25 pi) pbDist)
                                        ; upper right
                                  Filter
                                  )     ;_ ssget
                    )                   ; match filter, if any
            (cons (ssname ssPick 0) (list PtPick))
            )                           ;_ if
          )
        )                               ;_ cond
  )                             ; then return first entity as (entsel)
;;;
;;;-------------------------------------------------------------------
;;;
;; This function is freeware courtesy of the author's of "Inside AutoLisp"
;; for rel. 10 published by New Riders Publications.  This credit must
;; accompany all copies of this function.
;;
;;* UDIST User interface function
;;* BIT (0 for none) and KWD key word ("" for none) are same as for INITGET.
;;* MSG is the prompt string, to which a default real is added as <DEF> (nil
;;* for none), and a : is added. BPT is base point (nil for none).
;;*
(defun F:rb-UDIST (bit kwd msg def bpt / inp)
  (if def
    (setq msg (strcat "\n" msg " <" (rtos def) ">: ")
          bit (* 2 (fix (/ bit 2)))
          )                             ;setq
    (setq msg (strcat "\n" msg ": "))
    )                                   ;if
  (initget bit kwd)
  (setq inp
          (if bpt
            (getdist msg bpt)
            (getdist msg)
            ) )                         ;setq & if
  (if inp inp def)
  )                                     ;defun
;;;
;;;-------------------------------------------------------------------
;;;
;; This function is freeware courtesy of the author's of "Inside AutoLisp"
;; for rel. 10 published by New Riders Publications.  This credit must
;; accompany all copies of this function.
;;
;;* UKWORD User key word. DEF, if any, must match one of the KWD strings
;;* BIT (1 for no null, 0 for none) and KWD key word ("" for none) are same as
;;* for INITGET. MSG is the prompt string, to which a default string is added
;;* as <DEF> (nil or "" for none), and a : is added.
;;*
(defun F:rb-UKWORD (bit kwd msg def / inp)
  (if (and def (/= def ""))
    (setq msg (strcat "\n" msg " <" def ">: ")
          bit (* 2 (fix (/ bit 2)))
          )                             ;setq
    )                                   ;if
  (initget bit kwd)
  (setq inp (getkword msg))
  (if inp inp def)
  )                                     ;defun
;;;
;;;-------------------------------------------------------------------
;;;
;; This function is freeware courtesy of the author's of "Inside AutoLisp"
;; for rel. 10 published by New Riders Publications.  This credit must
;; accompany all copies of this function.
;;
;;* UREAL User interface real function
;;* BIT (0 for none) and KWD key word ("" for none) are same as for INITGET.
;;* MSG is the prompt string, to which a default real is added as <DEF> (nil
;;* for none), and a : is added.
;;*
(defun F:rb-UREAL (bit kwd msg def / inp)
  (if def
    (setq msg (strcat "\n" msg " <" (rtos def 2) ">: ")
          bit (* 2 (fix (/ bit 2)))
          )
    (setq msg (strcat "\n" msg ": "))
    )                                   ;if
  (initget bit kwd)
  (setq inp (getreal msg))
  (if inp inp def)
  )                                     ;defun
;;;
;;;-------------------------------------------------------------------
;;;
;; test if polyline is closed
(defun F:rb-PL-CLOSED? (ent)
  (if (= 1 (logand 1 (F:rb-DXF 70 (entget ent)))) ; is a closed pline
    'T
    nil
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; get group code value
(defun F:rb-DXF (n alist) (cdr (assoc n alist)) )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 19.09.2005
;;
;; get point list of polyline
(defun F:rb-GET-POLY-PTS (ename / entlist typ alt )
  (setq entlist (entget ename) )
  (cond
    ((and
       (= (F:rb-DXF 0 entlist) "POLYLINE")
       (not (member (F:rb-DXF 70 entlist) '(8 9 12 13))) ; no 3D-Poly
       )
      ;; 0 = open,
      ;; 1 = closed
      ;; 2 = curved (open)
      ;; 3 = curved (closed)
      ;; 4 = splined (open)
      ;; 5 = splined (closed)
      ;; 8 = 3D-Polyline (open)
      ;; 9 = 3D-Polyline (closed)
      ;;12 = 3D-Polyline, curved (open)
      ;;13 = 3D-Polyline, curved (closed)

      ;; GC 75 - Curves and Smooth Surface type (Optional)
      ;;         0 = No Smooth Surface Fitted
      ;;         5 = Quandratic B-Spline Surface                                                                                           6 = Cubic B-Spline Surface
      ;;         8 = Bezier Surface


      (if (member (F:rb-DXF 70 entlist) '(0 1))

        ;; returns a list of points on the entity
        (F:rb-GET-PL-POINT-LIST ename nil)

        (progn
          (if (not **CONT*)
            (progn
              (terpri)
              (while (not **CONT*)
                (setq alt
                        (getreal
                         "\r Arc segment error tolerance (altitude) <0.02 [0.01/0.005/0.001]: <default> "
                         )
                      )
                (if (or (not alt) (< 0 alt 0.02) )
                  (setq **ALT*  alt
                        **CONT* 'T ; ask for arc tolerance only one time
                        )
                  (progn
                    (setq alt nil
                          **ALT*  nil
                          **CONT* nil
                          )
                    (princ "\n Tolerance should be positiv and < 0.02\n")
                    )
                  )

                )                       ; while
              (princ "\n   Arc segment error tolerance: ")
              (if **ALT*
                (princ **ALT*)
                (princ "default")
                )
              )
            )
          ;; returns a list of points on the entity
          ;; **ALT* -> is an arc segment error tolerance (altitude)
          (F:rb-GET-PL-POINT-LIST ename **ALT*)
          )
        )
      )
    ((= (F:rb-DXF 0 entlist) "LWPOLYLINE")
      (F:rb-GET-PL-POINT-LIST ename nil) ; returns a list of points on the entity
      )
    ('T (princ "\n Error, line is not a (LW)Polyline\n") (exit) )
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 06.12.2005
;;
(defun F:rb-GET-PL-POINT-LIST ( en alt / a b c flag p1 p2 p3 e1 lst lst2 n en2 transcode z)

  (if (= (type en) 'LIST)
    (setq transcode (cadr en)
          en        (car  en)
          )
    (setq transcode 1)
    )

  (setq e1 (entget en))

  (if (equal 1 (logand 1 (F:rb-DXF 70 e1)))
    (setq flag 'T)
    (setq flag nil)
    )

  (setq en2 en)

  (if (equal (F:rb-DXF 0 e1) "POLYLINE")
    (progn
      (setq en (entnext en)
            e1 (entget en)
            p1 (F:rb-DXF 10 e1)
            b  (F:rb-DXF 42 e1)
            )

      (if (not (equal 16 (logand 16 (F:rb-DXF 70 e1))))
        (setq lst (list p1))
        (setq lst '())
        )

      (setq en (entnext en)
            e1 (entget en)
            )

      (while (/= "SEQEND" (F:rb-DXF 0 e1 ))
        (setq p2 (F:rb-DXF 10 e1))

        ;; not a spline control point
        (if (not (equal 16 (logand 16 (F:rb-DXF 70 e1))))
          (progn
            (if (and b
                     (not (equal b 0))
                     p1
                     (not (equal p1 p2 0))
                     )
              (setq p3  (F:rb-GET-PL-ARC-DATA p1 p2 b)
                    lst (append lst
                                (F:rb-GET-ARC-POINTS (car p3) p1 p2 (caddr p3) alt)
                                )
                    )
              (setq lst (append lst (list p2)))
              )
            )
          )

        (setq b  (F:rb-DXF 42 e1)
              en (entnext en)
              e1 (entget en)
              p1 p2
              )
        )                               ; while

      (if flag                          ; closed Polyline
        (progn
          (setq p2 (car lst))
          (if (and b
                   (not (equal b 0))
                   p1
                   (not (equal p1 p2 0))
                   )
            (setq p3  (F:rb-GET-PL-ARC-DATA p1 p2 b)
                  lst (append lst
                              (F:rb-GET-ARC-POINTS (car p3) p1 p2 (caddr p3) alt)
                              )
                  )
            (setq lst (append lst (list p2)))
            )
          (setq flag nil)
          )
        )
      )

    ;; LWPolyline
    (progn
      (setq z   (F:rb-DXF 38 e1)
            lst (member (assoc 10 e1) e1)
            )
      (if (not z) (setq z 0.0))
      (setq n 0)
      (repeat (length lst)
              (setq a (nth n lst))
              (if (equal 10 (car a))
                (setq a    (cons 10 (append (cdr a) (list z)))
                      lst2 (append lst2 (list a))
                      )
                (progn
                  (if (equal 42 (car a))
                    (setq lst2 (append lst2 (list a)))
                    )
                  )
                )
              (setq n (1+ n))
              )

      (setq b   (car lst2)
            lst (list (cdr b))
            )

      (if flag
        (setq lst2 (append lst2 (list (car lst2)))
              flag nil
              )
        )

      (setq n 1)
      (repeat (- (length lst2) 1)
              (setq a (nth n lst2))

              ;;----------------------------------
              (if (and (equal 10 (car a))
                       (equal 42 (car b))
                       (not (equal 0.0 (cdr b)))
                       c
                       (not (equal (cdr a) (cdr c)))
                       )
                ;; Get the arc points
                (setq  p3 (F:rb-GET-PL-ARC-DATA (cdr c) (cdr a) (cdr b))
                      lst (append lst
                                  (F:rb-GET-ARC-POINTS
                                   (car p3)
                                   (cdr c)
                                   (cdr a)
                                   (caddr p3)
                                   alt
                                   )
                                  )
                      )
                (if (equal 10 (car a))
                  (setq lst (append lst (list (cdr a))) )
                  )
                )
              ;;----------------------------------

              (setq c b
                    b a
                    n (1+ n)
                    )
              )
      (setq lst2 nil)
      )
    )

  (F:rb-LST-TRANS lst en2 transcode)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Takes endpoints of an arc and bulge
;; and returns a list containing:
;;   center point
;;   radius
;;   delta angle
;;   distance (arc length)
;;   altitude
;;
(defun F:rb-GET-PL-ARC-DATA ( p1 p2 bulge / alt arcl dist rad ang p3)
  (setq dist (distance p1 p2)
        alt  (abs (/ (* dist bulge) 2.0))
        rad  (/ (+ (expt (/ dist 2.0) 2.0)
                   (expt alt 2.0)
                   )
                (* 2.0 alt)
                )
        ang  (* 2.0 (atan (/ dist 2.0) (- rad alt) ) )
        arcl (* ang rad)                ; arc length
        ang  (* ang (/ bulge (abs bulge)))
        p3   (polar p1 (angle p1 p2) (/ dist 2.0))
        p3   (polar
              p3
              (+ (angle p1 p2) (/ pi 2.0))
              (* (- rad alt) (/ bulge (abs bulge)) )
              )
        )
  (list p3 rad ang arcl alt)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; F:rb-GET-ARC-POINTS
;; returns a list of points that lie along an arc described by:
;;   p1 - start point
;;   p2 - end point
;;   p3 - center point
;;  ang - delta angle of the arc
;;  alt - altitude (max. error tolerance)
;;
(defun F:rb-GET-ARC-POINTS ( p1 p2 p3 ang alt / da sa ea lst rad res)

  (setq rad (distance p1 p2)            ; radius
        sa  (angle p1 p2)               ; start angle
        ea  (+ sa ang)                  ; end angle
        da  (- ea sa)         ; full delta angle of the arc in radians
        )

  (if (not alt)
    (setq res (/ (* 2.0 pi) 9.0))       ; default resolution
    (setq res (F:rb-GET-DELTA-ANG rad alt)) ; altitude specified.
    )

  (setq  res (* res (/ da (abs da)))) ; delta angle increment of the loop

  (if (< (abs (/ da res)) 4.0)
    (setq res (/ da 4.0) ) ; reset resolution, the delta angle increment of the loop so
                                        ; at least 4 segments are used
    )

  (repeat (+ (fix (+ (abs (/ da res)) 0.000001 ) ) 1 )
          (if (not (equal (polar p1 sa rad) (last lst)))
            (setq lst (append lst (list (polar p1 sa rad)) ) )
            )
          (setq sa (+ sa res))
          )                             ; repeat

  (if (not (equal p3 (last lst)))
    (setq lst (append lst (list p3)))
    )
  lst
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; F:rb-GET-DELTA-ANG
;; returns the delta angle of an arc with
;; the specified altitude and radius
;;
(defun F:rb-GET-DELTA-ANG ( rad alt / ang tmp)
  (setq tmp (* 2.0
               (sqrt
                (abs (- (* 2.0 rad alt)
                        (expt alt 2.0)
                        ))))
        ang (* 2.0 (atan (/ tmp 2.0) (- rad alt) ) )
        )
  ang
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; F:rb-LST-TRANS
;; returns a list of translated points
;;
(defun F:rb-LST-TRANS ( lst a b / lst2 p n)
  (setq n 0)
  (repeat (length lst)
          (setq p    (nth n lst)
                p    (trans p a b)
                lst2 (cons p lst2)
                n    (1+ n)
                )
          )
  (reverse lst2)
  )
;;;
;;;----------------------------------------------------------------
;;;
;; make list of lists with 2 points out of vertex list
(defun F:rb-MAKE-PT-PAIRS (vlist / seglist)

  ;; get the list of segments
  (while (cadr vlist)              ; there is a vertex before this one
    (setq seglist (cons         ; add this segment to the segment list
                   (list
                    (car vlist)         ; the vertex
                    (cadr vlist)        ; the vertex before
                    )
                   seglist
                   )
          )
    (setq vlist (cdr vlist) )
    )
  (reverse seglist)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; `incr' is distance to increase size of balls, USERR2 stores the rest
;; distance of increment which does not fit on current segment, the rest
;; distance will be the start distance on the next segment.
;;
(defun F:rb-GET-INCR (incr line ptFirst ptLast / incrlst L_Ang L_len rest runlth )
  (setq L_Ang (apply 'angle line)       ; line angle
        L_len (apply 'distance line)    ; line length
        )

  (if ptFirst
    (setq incrlst (list (car line))) ; start with the first segment pt.
    (setq incrlst '())                  ; or with a empty list
    )

  (cond
    ;; If in the previous run the increment along 1. border
    ;; did not fit completely on the previous line segment
    ;; start the current run with the rest of the increment.
    ((> (getvar "USERR2") 0) (setq runlth (getvar "USERR2")))
    ('T (setq runlth incr))
    )

  (if (> incr 0)
    (while (< runlth L_len)
      (setq incrlst (cons (polar (car line) L_Ang runlth) incrlst)
            runlth  (+ runlth incr)
            )
      )
    )

  ;; Revision 23.11.2005
  (if ptLast
    ;; add endpoint of line to list if it is not in list.
    (setq incrlst (F:rb-ADJOIN (cadr line) incrlst))
    )

  ;; save rest of increment which doesn't fit on segment
  ;; for to start with on the next segment.
  (setvar "USERR2" (- runlth L_len))

  (reverse incrlst)
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-VIEWSAVE ( flag / viewname)
  (if *debug*
    (progn
      (setq viewname "$$_ROLLING_BALL_VIEW_SAVE_$$")
      (cond
        ((= flag 1)                     ; delete & save
          (if (tblsearch "VIEW" viewname)
            (command "_view" "_d" viewname)
            )
          (command "_view" "_save" viewname)
          )
        ((= flag 2)                     ; restore & delete
          (if (tblsearch "VIEW" viewname)
            (progn
              (command "_view" "_r" viewname)
              (command "_view" "_d" viewname)
              )
            )
          )
        ((= flag 3)                     ; delete only
          (if (tblsearch "VIEW" viewname)
            (command "_view" "_d" viewname)
            )
          )
        )
      )
    )
  )
;;;
;;;--- Error ---------------------------------------------------------
;;;
;;(vl-load-com)
(defun F:rb-ERROR ( msg / errn)
  (princ "\n -- F:rb-ERROR --")
  (setq errn (getvar "ERRNO"))
  ;;(vl-bt)                               ; backtrace
  (princ "\n Resetting program changes ...")

  (while (not (wcmatch (getvar "cmdnames") "*UNDO*")) (command "_.undo") )

  ;; The routine that just failed created an undo
  ;; begin mark, so we need to close it off with
  ;; and "end" mark.
  (command "_end")

  ;; Back up to the beginning.
  (command "_.undo" "1")
  (while (not (equal (getvar "cmdnames") "")) (command nil) )

  (princ "\n Error No.: ")
  (princ errn)
  (princ (strcat "\n " msg))

  ;; remove highlight from selected polylines
  (if **SEL* (foreach x **SEL* (redraw x 4)))

  (F:rb-RESET nil)
  (princ "\n -- F:rb-ERROR --")
  )
;;;
;;;--- Globals -------------------------------------------------------
;;;
;; set global vars 'nil
(defun F:rb-SET-NIL-GLOBAL-VARS ()
  (mapcar '(lambda (x) (set x nil))
          (list
           '**ALIST*                    ; points of 1. boundary
           '**ALT* ;..................... arc tolerance in function F:rb-GET-POLY-PTS (altitude)
           '**ANSLIST* ;................. answer list with circle center & radius
           '**BLIST-SPLIT* ;............. points of 2. boundary, split into sublists
           '**BLIST-UNSPLIT* ;........... points of 2. boundary, sublists not split
           '**BLIST-UNSPLIT-LEN*        ; list length
           '**CENT* ;.................... circle object or list with center & radius
           '**COL1* ;.................... copy of main color, for debug purpose
           '**CONT*                     ; flag
           '**CORRECTION-ERRORS*        ; direction number errors
           '**DIR1*                     ; direction number
           '**DIR2*                     ; direction number
           '**NOT-PERP-CPTS* ;........... list of circle centers not perp. to both boundaries
           '**OLD-CPT-NEW* ;........... copy of corrected center point
           '**OLD-INT1* ;................ copy of intersection on 1. border
           '**OLD-INT2-NEW* ;............ copy of corrected intersection on 2. border
           '**PERP-CPT-Int2* ;........... list with circle centers & inters. pts. on 2. boundary
           '**PREV-CPT*                 ; previous circle centerpoint
           '**S_TIME* ;.................. stores start time of total process
           '**TIME1* ;................... stores start time of a sub process
           '**SEL* ;..................... stores objectnames of the selected polylines
           )
          )
  )
;;;
;;;--- Reset ---------------------------------------------------------
;;;
(defun F:rb-RESET (undoEnd)

  ;; Set global vars nil
  (F:rb-SET-NIL-GLOBAL-VARS)

  ;; Reset system vars
  (F:rb-MODER)

  (if undoEnd (command "_undo" "_end"))

  ;; Restore view
  (F:rb-VIEWSAVE 3)

  (redraw)

  (if **OLDERR*
    (setq *error*       **OLDERR*
          **OLDERR*  nil
          )
    )

  (princ)
  )
;;;
;;;--- Setup ---------------------------------------------------------
;;;
(defun F:rb-SETUP ( / tmp)

  (if F:rb-ERROR
    (setq **OLDERR* *error*
          *error*       F:rb-ERROR
          )
    )

  (redraw)                              ; clear screen

  (setq **CORRECTION-ERRORS* 0 ; counter for not corrected center points
        **NOT-PERP-CPTS*     0 ; counter for not perpendicular center points
        )


  (setq tmp (getvar "CMDECHO"))

  (setvar "CMDECHO" 0)
  (command "_undo" "_auto" "_on")
  (command "_undo" "_mark")
  (command "_undo" "_begin")
  (F:rb-VIEWSAVE 1)                     ; save view
  (command "_ucs" "_world")             ; set UCS to WCS
  (setvar "CMDECHO" tmp)

  ;; Save system vars
  (F:rb-MODES
   (list

    ;; Will NOT be changed in this function
    "CECOLOR"                           ; saved
    "CLAYER"                            ; saved
    "ERRNO"                             ; saved

    ;; Will be changed in this function
    "AUPREC"                            ; saved
    "CMDECHO"                           ; saved
    "COORDS"                            ; saved
    "DIMADEC"                           ; saved
    "DIMZIN"                            ; saved
    "LOGFILEMODE"                       ; saved
    "LUNITS"                            ; saved
    "LUPREC"                            ; saved
    "ORTHOMODE"                         ; saved
    "OSMODE"                            ; saved
    "PICKBOX"                           ; saved
    "REGENMODE"                         ; saved
    "UCSICON"                           ; saved

    "USERI1"                            ; saved
    "USERI2"                            ; saved
    "USERI3"                            ; saved
    "USERI4"                            ; saved
    "USERI5"                            ; saved

    ;;"USERR1"                          ; not saved
    "USERR2"                            ; saved
    ;;"USERR3"                          ; not saved
    ;;"USERR4"                          ; not saved
    ;;"USERR5"                          ; not saved

    "USERS1"                            ; saved
    "USERS2"                            ; saved
    "USERS3"                            ; saved
    "USERS4"                            ; saved
    ;;"USERS5"                          ; not saved
    )
   )

  ;; SET SYSTEM & USER VARS
  (mapcar '(lambda (x) (setvar (car x) (cadr x)) )
          (list
           ;; --------------------------------------------------------
           ;;  SYSTEM Vars
           ;; --------------------------------------------------------
           '("AUPREC"       8)
           '("CMDECHO"      0)
           '("COORDS"       1)
           '("DIMADEC"      8)
           '("DIMZIN"       8)
           '("ERRNO"        0)
           '("LOGFILEMODE"  0)
           '("LUPREC"       4)
           '("ORTHOMODE"    0)
           '("OSMODE"       0)
           '("PICKBOX"      5)
           '("REGENMODE"    1)
           '("UCSICON"      0)

           ;; --------------------------------------------------------
           ;; USER Vars
           ;; --------------------------------------------------------
           '("USERI1" 0) ; "userI1" - stores length of list A (first boundary)
           '("USERI2" 0) ; "userI2" - stores length of list B (second boundary)
           '("USERI3" 0) ; "userI3" - stores 0 or -1 for lowering index number
           '("USERI4" 0) ; "userI4" - stores index number of last used sublist
           '("USERI5" 0) ; "userI5" - count center points .............

           ;;........... ; "userR1" - no start value, stores user input of increment along 1. boundary
           '("USERR2" 0) ; "userR2" - stores rest distance of segment increment along 1. boundary
           ;;........... ; "userR3" - no start value, stores multiplier for Part Angles
           ;;........... ; "userR4" - no start value, stores factor for calculating ball increase/decr.
           ;;........... ; "userR5" - no start value,used for debugging purpose

           '("USERS1" "") ; "userS1" - stores program name, ROLLIN or ROLLIN2
           ;;............ ; "userS2" - NOT USED
           ;;............ ; "userS3" - no start value, used for DEBUGGING
           ;;............ ; "userS4" - no start value, used for DEBUGGING
           ;;............ ; "userS5" - no start value, used as counter in DEBUG function
           )
          )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Save system vars
(defun F:rb-MODES (a)
  (setq **SYSVAR-LST* '())
  (repeat (length a)
          (setq **SYSVAR-LST*
                  (append **SYSVAR-LST* (list (list (car a) (getvar (car a)))))
                )
          (setq a (cdr a))
          )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Reset system vars
(defun F:rb-MODER ()
  (repeat (length **SYSVAR-LST*)
          (setvar (caar **SYSVAR-LST*) (cadar **SYSVAR-LST*))
          (setq **SYSVAR-LST* (cdr **SYSVAR-LST*))
          )
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-SELECT-BORDERS ( / ent1s ent2s)
  (setq **SEL* nil)
  (setq ent1s
          (F:rb-EntSelF
           "\n Choose the starting end of the first Polyline Boundary:"
           '((0 . "POLYLINE,LWPOLYLINE"))
           )
        )

  (if (not ent1s)
    (princ "\n Wrong first Object or no selection")
    )

  (if ent1s
    (progn
      ;; highlight first boundary
      (redraw (car ent1s) 3)

      ;; save objectname to redraw in case of error
      (setq **SEL* (cons (car ent1s) **SEL*))

      (setq ent2s
              (F:rb-EntSelF
               "\n Choose the starting end of the second Polyline Boundary:"
               '((0 . "POLYLINE,LWPOLYLINE"))
               )
            )
      )
    )

  (if (and ent1s (or (not ent2s) (eq (car ent1s)(car ent2s))))
    (progn
      (setq ent2s nil)
      (princ "\n Wrong second Object or no selection")
      )
    )

  ;; highlight second boundary
  (if ent2s (redraw (car ent2s) 3) )

  (if (and ent1s ent2s)
    (progn
      ;; save objectname to redraw in case of error
      (setq **SEL* (cons (car ent2s) **SEL*))
      (list ent1s ent2s)
      )
    nil
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-DRAW-PLINE (msg lst closed / tmp)
  (princ (strcat "\n " msg))
  (if **3D*
    (command "_3dpoly")
    (command "_pline")
    )
  (mapcar '(lambda (p)
            (princ (strcat "\r " (setq tmp (F:rb-SPIN tmp))))
            (command p)
            )
          lst
          )
  (if closed
    (command "_c")
    (command "")
    )
  (princ (strcat "\r " msg " DONE"))
  )
;;;
;;;----------------------------------------------------------------
;;;
;; make plural out of singular if <n>umber is greater 1
(defun F:rb-GRAMMAR (word n / ch)

  (if (= (type n) 'STR) (setq n (atoi n)) )

  (cond
    ((and (> n 1) (= (strcase word) "IS") )
      (setq word "are" ch "" )
      )
    ;; If it's only 1, leave it singular
    ((= n 1) (setq ch "") )

    ;; If no grammar correction and <n>umber greater 1, add `s' for plural
    ((> n 1) (setq ch "s") )

    ;; If it's 0 or < 0, leave it singular
    ('T (setq ch ""))
    )

  (strcat word ch)                      ; return the corrected word
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-SET-LAYER (LAYERNAME COL setlayer)
  (if (tblsearch "LAYER" LAYERNAME)
    (command "_layer" "_thaw" LAYERNAME "_on" LAYERNAME "")
    (command "_layer" "_make" LAYERNAME "_color" COL LAYERNAME "")
    )
  (if setlayer
    (progn
      (command "_layer" "_set" LAYERNAME "")
      ;;(setvar "CLAYER" LAYERNAME)
      (cond
        ((and (= (type COL) 'STR) (wcmatch COL "*LAYER*"))
          (setvar "CECOLOR" "256")
          )
        ((and (= (type COL) 'STR) (wcmatch COL "*BLOCK*"))
          (setvar "CECOLOR" "255")
          )
        ('T
          (setvar "CECOLOR" (if (numberp col) (itoa col) col))
          )
        )
      )
    )
  (princ)                               ; no nil
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Force the angle (radians) 0 <= ang < 2pi
;; Reduces to [0, 360∞)
;; Takes an angle in radians and reduces it to less than 2pi if ang>=2pi
(defun F:rb-AngleFix (Ang)              ; 6.28319 = pi * 2
  (cond
    ((minusp Ang)
      (+ (* pi 2.0) Ang)
      )
    ((> Ang (* pi 2.0))
      (- Ang (* pi 2.0))
      )
    ('T Ang )
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Set number of Part Angles, overrides standard, to get the standard
;; back enter 1.
(defun F:rb-SETPA ( / tmp)
  ;; Get number of Part Angles
  (while (or (not tmp)(< tmp 1)(> tmp 100))
    (setq tmp
            (F:rb-UREAL
             1
             ""
             "\n Number of Part Angles for ang. > 180∞ (2-100, 1 = use standard)"
             (if (< 0 (fix (getvar "USERR3")) 101)
               (fix (getvar "USERR3"))
               1
               )
             )
          )
    )
  (if (> tmp 0)
    (setvar "USERR3" (fix tmp))
    )
  (if (> (fix tmp) 1)
    (princ (strcat "   Number of Part Angles for angles > 180∞ : " (rtos (fix tmp)) "\n"))
    (princ "   Program uses it's standard Part Angle numbers\n")
    )
  (princ)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; explodes the sublists created by F:rb-SPLIT-LIST down to single '(x y z) lists.
(defun F:rb-UNSPLIT-LIST (lst / unsplit sublist tmp)
  (setq tmp lst unsplit nil)
  (while tmp
    (setq sublist (car tmp)
          tmp     (cdr tmp)
          )
    (while sublist
      (setq unsplit (cons (car sublist) unsplit)
            sublist (cdr sublist)
            )
      )
    )
  (reverse unsplit)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; F:rb-SPLIT-LIST splits list into sublists of maximal length n
;; n must be > 0!
;; iterative version by serge pashkov, safer than recursive version
;;   (F:rb-SPLIT-LIST 2 '(1 2 3 4 5 6)) => ((1 2) (3 4) (5 6))
(defun F:rb-SPLIT-LIST (n lst / ret out cnt)
  (setq ret nil)                        ; possible vl lsa compiler bug

  ;; Adjust `cnt' to set incomplete number of elements (if any) for the
  ;; last segment
  (setq cnt (- n (rem (length lst) n))
        lst (reverse lst)
        )

  (while lst
    (setq ret (cons (car lst) ret)
          lst (cdr lst)
          )
    (if (zerop (rem (setq cnt (1+ cnt)) n))
      (setq out (cons ret out)
            ret nil
            )
      )
    )
  (if ret (cons ret out) out)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; List sort
(defun F:rb-LSORT (input compare / fun
                         ;; local functions
                         #LSORT-AUX #LSORT-SPLIT #LSORT-MERGE
                         )
  (defun #LSORT-AUX (input)
    (if (cdr input)
      ((lambda (lst)
         (#LSORT-MERGE
          (#LSORT-AUX (car lst))
          (#LSORT-AUX (cadr lst))
          )
         )
       (#LSORT-SPLIT input)
       )
      input
      )
    )

  (defun #LSORT-SPLIT (right / left)
    (repeat (/ (length right) 2)
            (setq left (cons (car right) left)
                  right (cdr right)
                  )
            )
    (list left right)
    )

  (defun #LSORT-MERGE (left right / out)
    (while (and left right)
      (if (apply fun (list (car left) (car right)))
        (setq out (cons (car left) out)
              left (cdr left)
              )
        (setq out (cons (car right) out)
              right (cdr right)
              )
        )
      )
    (append (reverse out) left right)
    )

  (setq fun (cond (compare) ('T '>)))
  (#LSORT-AUX input)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 14.12.2005
;;
;; Sorts a list of points by their distance from a given point.
(defun F:rb-SORT-BY-DISTANCE (ptlist basePt)
  (mapcar 'cadr   ; return point list `ptlst' without distance numbers
          (F:rb-LSORT
           ;; List to sort, distances `basePt' - `pt' are added to list `ptlst'
           (mapcar '(lambda (pt) (list (distance basePt pt) pt) ) ptlist )

           ;; compares the first number of two lists
           '(lambda (a b) (< (car a) (car b))) ;_ compare expression
           )                            ;_ sort function
          )                             ;_ mapcar
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 27.11.2005
;;
;; return is a list of points located on the same line segment (without double elements).
(defun F:rb-ADD-SWEEP-PTS (pt incr line lst1 lst2 / lst3)
  (cond
    ((not lst1)
      ;; sort point list by distance from base `pt'
      (F:rb-SORT-BY-DISTANCE lst2 pt)
      )
    ('T
      ;; If ball increase is >0
      (if (and (> incr 0) (cadr lst1) lst2)
        (setq lst2
                ;; keep only points of LST2 which are on
                ;; the outer right and outer left side of LST1
                ;;
                ;; LST1       : ---+--+--+--+--+--+---
                ;; LST2 before: --+--+--+--+--+--+--+-
                ;; LST2 after : --+--+-----------+--+-
                ;;
                (F:rb-KEEP-IF
                 '(lambda (x)
                   (not (F:rb-ONLINE
                         (list
                          (cadr lst1)
                          (cadr (reverse lst1))
                          )
                         x
                         )
                    )
                   )
                 lst2
                 )
              )
        )

      ;; build one point list and remove double points
      (foreach p (append lst1 lst2) (setq lst3 (F:rb-ADJOIN p lst3) ) )

      ;; sort point list by distance from base `pt'
      (F:rb-SORT-BY-DISTANCE lst3 pt)
      )
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 14.12.2005
;;
;; Calculate points on segment of 1. border for `area sweeping'
;; for total included line angles < 180 degrees.
(defun F:rb-GET-AREA-SWEEP-PTS_<_180 ( pt0 pt1 incr LTangle / dist ptlst tmp )

  ;; Line distances
  (setq dist (distance pt0 pt1))

  (foreach n (F:rb-GET-INTEGER-LIST LTangle) ; GET a list of INTEGER numbers
           (setq ptlst (cons
                        ;; Calculate points on line `pt0 - pt1'
                        (polar pt0 (angle pt0 pt1) (F:rb-PERCENT n dist))
                        ptlst
                        )))
  ptlst                                 ; return point list
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-PERCENT (x sum) (/ (* x sum) 100.0))
;;;
;;;-------------------------------------------------------------------
;;;
;; Real sequence, list of n equally spaced real numbers between
;; start and end (both including)
;; (F:rb-RSEQ 0 1 3) => (0.0 0.5 1.0)
(defun F:rb-RSEQ (start end n / lst d)
  (cond
    ((= n 1) start)
    ((< n 1) nil)
    ((< end start) nil)
    ('T
      (setq d (/ (float (- end start)) (1- n))
            lst nil
            )
      (repeat n
              (setq lst (cons (+ start (* (setq n (1- n)) d)) lst))
              )
      )
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; The list without the last element
(defun F:rb-BUTLAST (lst) (reverse (cdr (reverse lst))))
;;;
;;;-------------------------------------------------------------------
;;;
;; CALCULATE BALL ANGLE
(defun F:rb-GET-BALL-ANGLE (side line)
  (if (= "LEFT" side)
    (F:rb-AngleFix (+ (F:rb-AngleFix (apply 'angle line)) (/ pi 2.0) ))
    (F:rb-AngleFix (- (F:rb-AngleFix (apply 'angle line)) (/ pi 2.0) ))
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Remove double points from list
(defun F:rb-REMOVE-DOUBLE-PTS (lst / fuzz oldx tmp)
  ;; Check for double points
  (setq tmp  (reverse lst)
        lst  nil
        fuzz 1e-8
        )
  (foreach x tmp
           (if (not (equal oldx x fuzz))
             (setq lst (cons x lst))
             )
           (setq oldx x)
           )
  lst
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; 3 TESTS FOR DETECTING ERRORS IN CENTERLINE
;;
(defun F:rb-TEST-RESULT ( arglst / col1 ctrlist ctrlist2 errnlst flag
                                 lay1 lay2 lay3 layprefix n not-perp-cpts
                                 time1 tmp
                                 )

  (setq time1     (F:rb-GET-TIME)       ; set timer for error tests
        errnlst   '(1 10)               ; internal error numbers
        ctrlist   (nth 0 arglst)
        col1      (F:rb-DXF "COL1"      arglst)
        layprefix (F:rb-DXF "LAYPREFIX" arglst)
        lay1 (strcat layprefix "Error_Marks-RED" ) ; Center pt. not perpendicular to both boundaries
        lay2 (strcat layprefix "Error_Marks-YELLOW") ; Centerpoint position is not correct
        lay3 (strcat layprefix "Error_Marks-GREEN") ; Error marks (circles) on self intersecting mid-boundary
        )

  ;; --- 1. TEST ---
  ;; Checking whether there are center points that are not
  ;; perpendicular to both boundary intersection points.
  (princ "\n.")
  (princ "\r Verifying perpendicularity of Center points ...\r")
  (if (> **NOT-PERP-CPTS* 0)
    (progn
      ;; Search all error marked center points
      (setq NOT-PERP-CPTS
              (F:rb-KEEP-IF '(lambda (x) (member (F:rb-DXF "FLAG" x) errnlst))
                            **ANSLIST*
                            ))

      (F:rb-SET-LAYER lay1 col1 'T)     ; make ellipse layer
      (setvar "CECOLOR" "1")

      ;; mark error points with an ellipse
      (setq n 0)
      (foreach x NOT-PERP-CPTS
               (if (and
                     (not (equal (F:rb-DXF "CENTER" x) (F:rb-DXF "TAN2" x) 1e-8))
                     (member (F:rb-DXF "CENTER" x) ctrlist)
                     )
                 (progn
                   (command "_ellipse"
                            (F:rb-DXF "CENTER" x) (F:rb-DXF "TAN2" x)
                            (F:rb-PERCENT 5 (distance (F:rb-DXF "CENTER" x)
                                                      (F:rb-DXF "TAN2" x)
                                                      )))

                   (setq n (1+ n))
                   )
                 )
               )

      ;; Give report
      (if (> n 0)
        (princ
         (strcat "\n  *** CAUTION - "
                 (itoa n) " "
                 (F:rb-GRAMMAR "point" n) " in Centerline "
                 (F:rb-GRAMMAR "is"    n) " not perpendicular"
                 "\n  ............. to both boundary intersection points,"
                 "\n  ............. point "
                 (F:rb-GRAMMAR "position" n) " "
                 (F:rb-GRAMMAR "is"       n) " marked with a RED ellipse."
                 "\n  ............. A change of increment along 1. border or a change"
                 "\n  ............. of the ball increase factor might avoid this effect."
                 )
         )
        )
      )
    (princ "\r Verifying perpendicularity of Center points ... OK\r")
    )

  ;; --- 2. TEST ---
  ;; Check circle center points position by checking whether radius
  ;; lines `center <-> hit_point_on_2. boundary' intersect on themselves.
  (princ "\n.")
  (princ "\r Verifying position of perpendicular Center points ...\r")
  (setq **PERP-CPT-Int2*

          ;; Keep only valid data
          (F:rb-KEEP-IF '(lambda (x)
                          (not (member (F:rb-DXF "FLAG" x) errnlst))
                          )
                        **ANSLIST*
                        )

        ;; Extraction of center and 2. hit point
        **PERP-CPT-Int2*
          (mapcar '(lambda (x) (list (F:rb-DXF "CENTER" x) (F:rb-DXF "TAN2" x)) )
                  **PERP-CPT-Int2*
                  )
        )

  ;; Mark wrong center points
  (if (setq tmp (F:rb-VERIFY-CENTER-POS **PERP-CPT-Int2*))
    (progn

      (F:rb-SET-LAYER lay2 col1 'T)     ; make ellipse layer
      (setvar "CECOLOR" "2")

      ;; mark points with an ellipse
      (setq n 0)
      (foreach x tmp
               (if (and
                     (not (equal (car x)(cadr x) 1e-8))
                     (member (car x) ctrlist)
                     )
                 (progn
                   (command "_ellipse" (car x)(cadr x)
                            (F:rb-PERCENT 5 (apply 'distance x))
                            )
                   (setq n (1+ n))
                   )
                 )
               )

      ;; Give report
      (if (> n 0)
        (princ
         (strcat "\n  *** CAUTION - program recognized "
                 (itoa n) " point position "
                 (F:rb-GRAMMAR "error" n) " "
                 (F:rb-GRAMMAR "is"    n) " in Centerline,"
                 "\n  ............. point "
                 (F:rb-GRAMMAR "position" n) " "
                 (F:rb-GRAMMAR "is"       n) " marked with a YELLOW ellipse."
                 "\n  ............. A change of increment along 1. border or a change"
                 "\n  ............. of the ball increase factor might avoid this effect."
                 )
         )
        )
      )
    (princ "\r Verifying position of perpendicular Center points ... OK\r")
    )

  ;; --- 3. TEST ---
  ;; Check centerline for self intersecting segments
  (setq tmp      (F:rb-VERIFY-CENTERLINE (reverse ctrlist) lay3 col1)
        flag     (car  tmp)
        ctrlist2 (cadr tmp)
        )

  (princ "\n Elapsed Time for tests: ")
  (princ (- (F:rb-GET-TIME) time1))     ; elapsed time for error tests

  (list ctrlist2 flag)                  ; return
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-PRINT-CHAR (ch n)
  (princ "\n")
  (repeat n (princ ch))
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Fill a string on the left side with a certain char
(defun F:rb-STR-LPADCHAR (str nl ch / )
  (cond
    ((= (strlen str) nl) str)
    ((> (strlen str) nl) (substr str 1 nl))
    ('T (F:rb-STR-LPADCHAR (strcat ch str) nl ch))
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-DIVIDE-SEGMENTS (lst / tmp oldp)
  (setq tmp (cons (car lst) tmp))
  (foreach p lst
           (if oldp
             (setq tmp (cons (F:rb-MIDPT oldp p) tmp)
                   tmp (cons p tmp)
                   )
             )
           (setq oldp p)
           )
  (reverse tmp)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; F:rb-ROTATE-LEFT rotates a list leftwise
;; put the first element to the end, 2x faster than ROTATE-RIGHT
;; (F:rb-ROTATE-LEFT '(0 1 2 3)) => '(1 2 3 0)
(defun F:rb-ROTATE-LEFT (lst) (append (cdr lst) (list (car lst))))
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 14.12.2005
;; This function searches the point in a list that is closest to a
;; supplied point and sets this point as the start point of the list.
;; PT must be a point and LST must be a list of points.
(defun F:rb-ROTATE-LST-TO-PT (lst pt / cp)
  (setq cp (F:rb-CLOSEST-PT pt lst))
  (princ "\n   changing list order ...\r")
  (while (not (equal (car lst) cp))
    (setq lst (F:rb-ROTATE-LEFT lst))
    )
  (F:rb-CLEAR-CMDLINE 50)
  lst
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; (F:rb-GRDRAW-POINT (car B) 2 "X")(getint)
;;
;; Get the start points of the lists close to each other
;; and try to make the point lists go into the same direction
(defun F:rb-ALIGN-LISTS ( A B Apick Aclosed Bclosed / tmp
                            #CLOSE-LST #REVERSE-LST?
                            )
  (defun #CLOSE-LST (lst)
    (if (not (equal (car lst) (last lst) 0))
      (setq lst (append lst (list (car lst))))
      )
    lst
    )

  (defun #REVERSE-LST? (lst basePt)
    (if (< (distance (last lst) basePt)
           (distance (car  lst) basePt)
           )
      (setq lst (reverse lst))
      )
    lst
    )

  ;; If last vertex of 1. border is closer to pick point
  ;; start from that point.
  (if (not Aclosed) (setq A (#REVERSE-LST? A Apick)))

  ;; If last vertex of 2. border is closer to start point
  ;; of list A, start from that point, else start B from last vertex.
  (if (not Bclosed) (setq B (#REVERSE-LST? B (car A))))

  (cond
    ;;------------------------------------------------------
    ((and (not Aclosed) (not Bclosed))
      (princ "\n .\n")
      )
    ;;------------------------------------------------------
    ((and Aclosed Bclosed)
      ;; Open lists
      (setq A (F:rb-BUTLAST A))
      (setq B (F:rb-BUTLAST B))

      ;; Set the closest point of `B' to the start point
      ;; of list `A' as the new start point of list `B'.
      (setq A (F:rb-ROTATE-LST-TO-PT A Apick ))

      ;; Check direction of lists
      (if (/= (F:rb-POLY-ROT-CCW A) (F:rb-POLY-ROT-CCW B))
        (setq B (reverse B))
        )

      ;; Rotate list B until it's startpoint is close
      ;; to pick point on list A
      (setq B (F:rb-ROTATE-LST-TO-PT B Apick))

      ;; Close lists
      (setq A (#CLOSE-LST A))
      (setq B (#CLOSE-LST B))

      (princ "\n ..\n")
      )
    ;;------------------------------------------------------
    ((and Aclosed (not Bclosed))
      (setq Aclosed nil
            A        (F:rb-ROTATE-LST-TO-PT A (car  B))
            tmp (car (F:rb-ROTATE-LST-TO-PT A (last B)))
            A   (reverse (member tmp (reverse A)))
            )

      (if (/= (F:rb-POLY-ROT-CCW A) (F:rb-POLY-ROT-CCW B))
        (setq B (reverse B))
        )

      (setq B (#REVERSE-LST? B  Apick  ))
      (setq A (#REVERSE-LST? A (car B) ))

      (princ "\n ...\n")
      )
    ;;------------------------------------------------------
    ((and (not Aclosed) Bclosed)
      (setq B (F:rb-ROTATE-LST-TO-PT B (car A) ))
      (setq B (#CLOSE-LST B))

      (if (/= (F:rb-POLY-ROT-CCW A) (F:rb-POLY-ROT-CCW B))
        (setq B (reverse B))
        )

      (princ "\n ....\n")
      )
    ('T (princ "\n *** ERROR ***\n"))
    )
  (list A B Aclosed)
  )
;;;
;;;-------------------------------------------------------------------
;;;
(defun F:rb-DRAW-RESULTS ( / ctrlist ctrlist2 flag n tmp x )

  ;; Build list of circle center points out of answer list
  (setq ctrlist (mapcar '(lambda (x)(F:rb-DXF "CENTER" x)) **ANSLIST*))

  (F:rb-PRINT-CHAR "-" 75)

  (if (= (length ctrlist) 0)
    (progn
      (F:rb-PRINT-CHAR "-" 75)
      (princ "\n Length of centerline list is 0")
      )
    (progn
      ;; Removing colinear Center points
      (setq ctrlist (F:rb-REMOVE-COLINEAR-PTS ctrlist 1e-08))

      (F:rb-PRINT-CHAR "-" 75)

      ;;------------------------------------------------------
      ;; 3 TESTS FOR DETECTING ERRORS IN CENTERLINE
      ;;------------------------------------------------------
      (setq tmp (F:rb-TEST-RESULT (list
                                   ctrlist
                                   (cons "COL1" col1)
                                   (cons "LAYPREFIX" layprefix)
                                   ))
            ctrlist2 (car  tmp)
            flag     (cadr tmp)
            )
      ;;------------------------------------------------------
      ;; Draw centerline, circles, circle radius lines
      ;;------------------------------------------------------
      (if (not ctrlist2)(setq ctrlist2 ctrlist))
      (if (not flag) (setq flag 0))
      (cond
        ((= 0 flag)                     ; centerline is OK
          ;; Make layer for Centerline
          (F:rb-SET-LAYER lay1 col1 'T)
          ;; Draw centerline
          (F:rb-DRAW-PLINE "drawing centerline ..." ctrlist2 Aclosed)
          )
        ((= 1 flag)                  ; centerline defect and corrected
          ;; Make layer for Centerline
          (F:rb-SET-LAYER lay4 col1 'T)
          ;; Draw a centerline WITHOUT self intersecting segments.
          (F:rb-DRAW-PLINE "drawing corrected centerline ..." ctrlist2 Aclosed)
          )
        ((= 2 flag)              ; centerline defect and NOT corrected
          ;; Make layer for Centerline
          (F:rb-SET-LAYER lay5 col1 'T)
          ;; Draw centerline WITH self intersecting segments.
          (F:rb-DRAW-PLINE "drawing centerline with self intersecting segments ..." ctrlist2 Aclosed)
          )
        )

      (command "_undo" "_end")

      (if (member flag '(0 1 2))
        (progn
          ;; get length of centerline
          (command "_area" "_o" (entlast))
          (princ "\n The total length of Centerline is: ")
          (princ (rtos (getvar "PERIMETER") 2 4))

          (setq **ANSLIST* (reverse **ANSLIST*) )
          ;;--------------------------------------------------
          (if (= "Y" (F:rb-UKWORD
                      1
                      "N Y"
                      "\n Do you want to GET the Circles? [Yes/No] "
                      "N"
                      )
                 )
            (progn
              (command "_undo" "_begin")

              ;; Make layer for Circles
              (F:rb-SET-LAYER lay2 col1 'T)

              ;; Draw circles
              (princ "\n   drawing circles ...")
              (setq n 0)
              (repeat (length **ANSLIST*)
                      ;;...............................................
                      (F:rb-CMDACTIVE-P)
                      (princ (strcat "\r " (setq tmp (F:rb-SPIN tmp))))
                      ;;...............................................
                      (setq x (nth n **ANSLIST*))
                      (if (member (F:rb-DXF "CENTER" x) ctrlist2)
                        (if (> (F:rb-DXF "RADIUS" x) 0)
                          (command "_circle" (F:rb-DXF "CENTER" x) (F:rb-DXF "RADIUS" x))
                          )
                        )
                      (setq n (1+ n))
                      (princ)
                      )
              (princ "\r  ")
              (princ "\n DONE")
              (command "_undo" "_end")
              )
            )
          ;;--------------------------------------------------
          (if (= "Y" (F:rb-UKWORD
                      1
                      "N Y"
                      "\n Do you want to GET the Circle radius lines? [Yes/No] "
                      "N"
                      )
                 )
            (progn
              (command "_undo" "_begin")

              ;; Make layer for Circle Radius Lines
              (F:rb-SET-LAYER lay3 col1 'T)

              ;; Draw circle radius line
              (princ "\n   drawing circle radius lines ...")
              (setq n 0)
              (repeat (length **ANSLIST*)
                      ;;...............................................
                      (F:rb-CMDACTIVE-P)
                      (princ (strcat "\r " (setq tmp (F:rb-SPIN tmp))))
                      ;;...............................................
                      (setq x (nth n **ANSLIST*))
                      (if (member (F:rb-DXF "CENTER" x) ctrlist2)
                        (if (and (> (distance (F:rb-DXF "CENTER" x) (F:rb-DXF "TAN1" x)) 1e-10)
                                 (> (distance (F:rb-DXF "CENTER" x) (F:rb-DXF "TAN2" x)) 1e-10)
                                 )
                          (command "_line"
                                   (F:rb-DXF "TAN1"   x) ; circle hit pt. on 1. boundary
                                   (F:rb-DXF "CENTER" x) ; circle center pt.
                                   (F:rb-DXF "TAN2"   x) ; circle hit pt. on 2. boundary
                                   ""
                                   )
                          )
                        )
                      (setq n (1+ n))
                      (princ)
                      )
              (command "_undo" "_end")
              (princ "\r  ")
              (princ "\n DONE")
              )
            )
          ;;------------------------------------------------------------------
          )                             ; progn
        )                               ; if (member flag ...)
      )                                 ; progn
    )                                   ; if (= (length...) ..)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 16.12.2005
;;
;; Calculate next decreased run length,
;; runlth    = circle radius
;; d_cpt_ipt = distance between circle center and intersection point on 2. border
;;
(defun F:rb-GET-DEC-RUNLTH (runlth d_cpt_ipt / numlst)
  (setq numlst '(2 4 8 16 32 64 128 256 512 1024 2048
                 4096 8192 16384 32768 65536 131072))

  (while (and numlst (> d_cpt_ipt (- runlth (/ runlth (car numlst) ))))
    (setq numlst (cdr numlst))
    )
  (- runlth (/ runlth (* 2.0 (if numlst (car numlst) 131072 ))))
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 13.12.2005
;;
;; Draws a circle from point `int1' at angle `ballang1' and updates
;; it's centerpoint and radius until the circle hits the 2. boundary.
;; Function works with only one circle which gets constantly modified.
;;
(defun F:rb-DRAW-BALL ( int1 ballang1 segline1 incrb Mlength col / cpt
                             cpt-new done done1 flag int2-new intdata
                             rad-new runlth runlth_>_0 tmp update-expr
                             valid-intdata
                             )

  ;; Set update expression of **CENT*, circle or list.
  (if (= (getvar "USERS1") "ROLLIN")    ; program name
    (setq update-expr '(F:rb-ENTMOD-CIRCLE **CENT* cpt runlth) ) ; circle
    (setq update-expr '(setq **CENT* (list cpt runlth))        ) ; list
    )

  (setq ;; Start with a very small increment
        runlth (cond              ; growing circle radius (run length)
                 ((< 1e-8 incrb) 1e-8)
                 ('T incrb)
                 )
        RUNLTH_>_0 'T                   ; flag for circle radius > 0
        )

  ;; INCREASE CIRCLE UNTIL IT HITS THE 2. BOUNDARY
  (while (not DONE)

    ;;oooooooooooooooooooooooooooooooooooooooooooooooooooooo
    ;;               CREATE or MODIFY CIRCLE
    ;;oooooooooooooooooooooooooooooooooooooooooooooooooooooo
    (setq cpt (polar int1 ballang1 runlth)) ; circle centerpoint

    (cond
      ;; Update circle entity/circle list with new centerpoint & radius
      (**CENT* (eval update-expr) )

      ;; No circle entity or circle list `**CENT*', create one.
      ('T
        (if (= (getvar "USERS1") "ROLLIN") ; USERS1 stores program name
          (progn
            ;; Create a CIRCLE ENTITY
            (command "_circle" "_2p" int1 (polar int1 ballang1 runlth))

            ;; Save circle entity in a global variable
            (setq **CENT* (entlast))
            )
          ;; Create a LIST with centerpoint & radius, no real circle.
          (setq **CENT* (list cpt runlth))
          )
        )
      )
    ;;oooooooooooooooooooooooooooooooooooooooooooooooooooooo

    (cond
      ;; Reached maximum radius, circle is drawn on the wrong side, stop loop `DONE'
      ((> runlth MLength)
        (if (= "Y" (F:rb-UKWORD
                    1
                    "N Y"
                    "\n *** Caution - Circle reached maximum radius, continue? [Yes/No] "
                    "N"
                    )
               )
          (setq DONE 'T )
          (exit)
          )
        )
      ;;-------------------------------------------------------;;
      ;; Search first intersection of circle with 2. boundary. ;;
      ;;-------------------------------------------------------;;
      ((setq intdata (F:rb-SEARCH-INTERS **CENT* int1 nil 2)) ; 2 = INCREASING circle
        ;; keep the intersection of circle with 2. boundary
        (if intdata (setq valid-intdata intdata) )

        ;; if `int1' is also located on 2. border segment
        ;; `int1' will also become `int2'.
        (if (and intdata (= (F:rb-DXF "DIST2" intdata) 0) )
          (setq runlth 0 ) ; if `runlth' = 0 no more search will be done.
          )

        ;;----------------------------------------------------------;;
        ;; IF INTERSECTION WITH 2. BOUNDARY HAS HAPPENED,           ;;
        ;; DECREASE THE CIRCLE UNTIL IT NO LONGER HITS 2. BOUNDARY  ;;
        ;;----------------------------------------------------------;;

        (while (not DONE1) ; search for an intersection while decreasing the circle
          ;;-----------------------------------------------
          ;; Calculate next decreased circle radius
          ;;-----------------------------------------------
          (setq runlth (F:rb-GET-DEC-RUNLTH
                        runlth          ; the last used circle radius
                        (F:rb-DXF "DIST2" valid-intdata) ; distance `cpt' - `int2'
                        ))

          ;;oooooooooooooooooooooooooooooooooooooooooooooooo
          ;;                 MODIFY CIRCLE
          ;;oooooooooooooooooooooooooooooooooooooooooooooooo
          (if (> runlth 0) ; if `runlth' <=0 `cpt' and 2. border segment are colinear.
            (progn
              (setq cpt (polar int1 ballang1 runlth)) ; circle centerpoint

              ;; UPDATE CIRCLE ENTITY/CIRCLE LIST WITH NEW CENTERPOINT & RADIUS
              (eval update-expr)
              )
            (setq RUNLTH_>_0 nil)      ; if circle radius <= 0, NOT OK
            )
          ;;oooooooooooooooooooooooooooooooooooooooooooooooo

          (cond
            ;;----------------------------------------------;;
            ;; Search again for an intersection of the      ;;
            ;; decreased circle with the 2. boundary.       ;;
            ;;----------------------------------------------;;
            ((and RUNLTH_>_0 intdata)
              (if (setq intdata (F:rb-SEARCH-INTERS **CENT* int1 nil 2))
                ;; keep the last intersection of circle with 2. boundary
                (setq valid-intdata intdata )
                )
              )
            ;;----------------------------------------------;;
            ;; If circle no longer intersects 2. boundary   ;;
            ;; but hits 1. boundary, drop circle center and ;;
            ;; continue with the next one from 1. boundary. ;;
            ;;----------------------------------------------;;
            ((and RUNLTH_>_0 (not intdata) (F:rb-SEARCH-INTERS **CENT* nil ballang1 1))
              (setq DONE1 'T            ; stop loop `DONE1'
                    DONE  'T            ; stop loop `DONE'
                    )
              )
            ;;----------------------------------------------;;
            ;; If circle no longer intersects 2. boundary   ;;
            ;; and also does not hit 1. boundary than add   ;;
            ;; the circle centerpoint from the last         ;;
            ;; intersection to centerline list.             ;;
            ;;----------------------------------------------;;
            ('T
              ;; if circle radius > 0, OK
              (if RUNLTH_>_0

                ;; Correct circle center position, radius & intersection pt. on 2. boundary,
                ;; make sure that centerpoint has the same perpendicular distance to both boundaries.
                ;;----------------------------------------------------
                (if (setq tmp (F:rb-CORRECT-CENTERPOINT segline1 int1 ballang1 valid-intdata col) )
                  (setq cpt-new  (nth 0 tmp) ; corrected centerpoint
                        rad-new  (nth 1 tmp) ; corrected radius
                        int2-new (nth 2 tmp) ; corrected hit point on 2. boundary
                        flag     (nth 3 tmp) ; debug number 1 or 10 if error, else correct
                        )
                  (setq cpt-new nil )
                  )

                ;; Else `int1' and 2. border segment are colinear.
                ;;----------------------------------------------------
                (setq cpt-new  int1     ; centerpoint
                      rad-new  0        ; radius
                      int2-new int1     ; hit point on 2. boundary
                      flag     0        ; debug number
                      )
                )

              (cond
                ((and cpt-new
                      ;; No double center points
                      (not (equal **PREV-CPT* cpt-new 1e-8))
                      )

                  ;; Saving data in answer list to be able to recreate all valid
                  ;; circles and radius lines from circle center to both boundaries.
                  (setq **ANSLIST* (cons
                                    (list
                                     (cons "CENTER" cpt-new ) ; circle center
                                     (cons "RADIUS" rad-new ) ; circle radius
                                     (cons "TAN1"   int1    ) ; circle hit point on 1. boundary
                                     (cons "TAN2"   int2-new) ; circle hit point on 2. boundary
                                     (cons "FLAG"   flag    ) ; debug number
                                     )
                                    **ANSLIST*
                                    ))

                  ;; Count centerline points
                  (setvar "USERI5" (1+ (getvar "USERI5")))

                  ;; Mark the centerline segment on screen
                  (if **PREV-CPT*
                    (progn
                      (grdraw **PREV-CPT* cpt-new col)
                      (princ)     ; use `(princ)' for a screen refresh
                      )
                    )

                  ;; Save centerline point for comparison
                  (setq **PREV-CPT* cpt-new )
                  )
                ('T nil )
                )

              (setq DONE1 'T ; valid circle center found, stop loop `DONE1'
                    DONE  'T ; and stop loop `DONE' .......................
                    )
              )                         ;_ 'T
            )                           ;_ cond

          )                   ;_ (while (not DONE1), decreasing circle
        )                     ;_ ((setq intdata .. , increasing circle
      ;;-------------------------------------------------------;;
      ;; If Circle hits 1. boundary, stop loop `DONE' and
      ;; continue with the next circle from 1. boundary.
      ;;-------------------------------------------------------;;
      ((F:rb-SEARCH-INTERS **CENT* nil ballang1 1)
        (setvar "USERI3" -1) ; lower sublist index number by one for the next run
        (setq DONE 'T)
        )
      ;;-------------------------------------------------------;;
      ;; ELSE loop `DONE' continues, Circle didn't hit
      ;; 2. boundary because radius size is still to small.
      ;; INCREASE circle radius.
      ;;-------------------------------------------------------;;
      ('T (setq runlth (+ runlth incrb)) )
      )																	;_ cond
    ;;(break "DRAW-BALL")
    )                                   ;_ while (not DONE)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 29.11.2005
;;
;; Returns a list of numbers, they are used to
;; calculate distances on the lines.
(defun F:rb-GET-INTEGER-LIST (LTangle)
  (if (> (getvar "USERR3") 2)
    (F:rb-RSEQ 1 75 (fix (+ 0.5 (/ (getvar "USERR3") 2) )))
    (cond
      ((<    0.0 LTangle 110.0) '(               25 50 75))
      ((<= 110.0 LTangle 150.0) '(          12.5 25 50 75))
      ((<= 150.0 LTangle 170.0) '(      7.5 12.5 25 50 75))
      ((<= 170.0 LTangle 175.0) '(    5 7.5 12.5 25 50 75))
      ((<  175.0 LTangle 180.0) '(2.5 5 7.5 12.5 25 50 75))
      ('T '(2.5 5.0 7.5 12.5 25 50 75)) ; upper limit = 75
      )
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Returns division number for total included ball angle > 180 degrees.
;;  `LTangle' = total included line angle in degrees.
;;  Valid part angle numbers should be >= 2
(defun F:rb-GET-PART-ANGLE-NUM_>180 (LTangle)

  (if (> (getvar "USERR3") 1)    ; overrides the number of Part Angles
    (cond
      ;; does not change part angles for
      ;; angles between 180.0 and 185.0 degree
      ((>= LTangle 185.0) (fix (getvar "USERR3"))) ; return
      ('T 2)
      )
    (cond
      ((<  180.0 LTangle 185.0)  2) ; return ( 2 part angles = draw 1 ball  from corner point)
      ((<= 185.0 LTangle 200.0)  4)
      ((<= 200.0 LTangle 225.0)  6)
      ((<= 225.0 LTangle 270.0)  8)
      ((<= 270.0 LTangle 315.0) 10)
      ((<= 315.0 LTangle 360.0) 12)
      ('T 0 )
      )
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Revision: 13.12.2005
;;
;; Calculates the ball angle for each point of first border
;;
(defun F:rb-BUILD-BALLANGLE-LIST ( Alist1 Aclosed incr side / ALIST2
                                          BALLANG1 BALLANG2 BTANGLE CH CNT DP
                                          LTANGLE PANG PANGNUM PT0 PT1 PT2
                                          SEGLINE1 SEGLINE2 TMP TMP1 TMP2
                                          fuzzy time1 LTangle1
                                          )

  (setq time1 (F:rb-GET-TIME)           ; save process start time
        fuzzy 1e-4            ; for total included segment line angles
        )

  ;; IF 1. & 2. BORDER ARE CLOSED POLYLINES ...
  (if Aclosed
    (progn
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                                       ;;
      ;;                          <---------                                   ;;
      ;;     | 1. part | 2. part  | 1. part | 2. part |                        ;;
      ;; ----O---------+----------O---------+---------O------                  ;;
      ;;     |                    |                   |                        ;;
      ;;     |         |          |         |         |                        ;;
      ;;     |        pt1        pt0       pt2        |                        ;;
      ;;     |                                        |                        ;;
      ;;     |<- last segment   ->|<- first segment ->|                        ;;
      ;;                                                                       ;;
      ;; ... copy the 1. part of the first segment to the end of the list,     ;;
      ;; needed for calculating `area sweeping' in order to have the segment   ;;
      ;; connection point `pt0' between point `pt1' & `pt2' for easy           ;;
      ;; calculation of the total included segment line angle between two      ;;
      ;; segments.                                                             ;;
      ;;                                                                       ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq tmp1   (car  Alist1)        ; first segment
            tmp2   (last Alist1)        ; last  segment

            ;; copy the 1. part of the first segment to the end of the list
            Alist1 (append Alist1 (list tmp1))

            ;; Add a helper segment in front of the first segment
            Alist1 (cons (list
                          (polar (car tmp1) (+ (apply 'angle tmp2) pi)
                                 (if (= incr 0) 1 incr ))
                          (car tmp1)
                          )
                         Alist1
                         )
            ;; First Total included Line angle
            LTangle1 (F:rb-ROUND
                      (F:rb-RTD
                       (F:rb-DXF side (F:rb-LTANGLE
                                       (car  tmp1) ; pt0
                                       (car  tmp2) ; pt1
                                       (last tmp1) ; pt2
                                       )
                                 )
                       )
                      fuzzy
                      )
            )
      )
    ;; ... ELSE START WITH THE FIRST HALF OF THE FIRST SEGMENT
    (progn
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                                        ;;
      ;; Process the first part of the first segment separately in order to     ;;
      ;; have afterwards always the polyline segment connection point `pt0'     ;;
      ;; between point `pt1' & `pt2' for easy calculation of the total included ;;
      ;; segment line angle.                                                    ;;
      ;;                                                                        ;;
      ;;                          o <- pt0 = connection point                   ;;
      ;;                         / \                                            ;;
      ;;                pt1  -> +   + <- pt2                                    ;;
      ;; 1. part of 1. seg. -> /     \                                          ;;
      ;;      Start point  -> o       o - + - o - + ...                         ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Increase the number of points on the first
      ;; half of the first segment
      ;; start with First pt. of segline: 'T
      ;;        add Last  pt. of segline: 'T
      (setq tmp1 (F:rb-GET-INCR incr (car Alist1) 'T 'T))

      ;; Calculate ball angle 1
      (setq ballang1 (F:rb-GET-BALL-ANGLE side (car Alist1)))

      ;; Save each point with its ball angle and segment part in list
      (foreach pt tmp1 (setq Alist2 (cons (list pt ballang1 (car Alist1)) Alist2)))

      ;; Remove first segment part from list
      (setq Alist1 (cdr Alist1))
      )
    )

  (while Alist1
    ;;.............................................................................
    (princ (strcat "\r " (setq ch (F:rb-SPIN ch)) " Building ball angle list ..."))
    ;;.............................................................................

    ;; Get segment part of 1. border
    (setq segline1 (car Alist1)
          pt1      (car segline1)
          pt0      (cadr segline1)
          )

    ;; Calculate ball angle 1
    (setq ballang1 (F:rb-GET-BALL-ANGLE side segline1))

    ;; Calculate total included line angle of 1. & 2. segment part
    (if (setq segline2 (cadr Alist1))
      (progn
        (setq pt2 (last segline2)
              ;; Get total included line angle in degrees
              LTangle (F:rb-ROUND
                       (F:rb-RTD
                        (F:rb-DXF side (F:rb-LTANGLE pt0 pt1 pt2)))
                       fuzzy
                       )
              )

        ;; Calculate ball angle 2
        (setq ballang2 (F:rb-GET-BALL-ANGLE side (list pt0 pt2)))
        )
      )

    (setq Alist1 (cddr Alist1))         ; remove the 2 point pairs
    ;;--------------------------------------------------------------;;

    (if (and segline1 (not segline2))
      (progn
        ;;----------------------------------------------------------;;
        ;;       When it is the last segment part                   ;;
        ;;----------------------------------------------------------;;

        ;; Increase the number of points on segment part 1
        ;; start with First pt. of segline: NIL
        ;;        add Last  pt. of segline: 'T
        (setq tmp1 (F:rb-GET-INCR incr segline1 NIL 'T))

        ;; Save each point with its ball angle and segment part in list
        (foreach pt tmp1 (setq Alist2 (cons (list pt ballang1 segline1) Alist2)) )
        )
      (cond
        ;;----------------------------------------------------------;;
        ;;                                                          ;;
        ;; "AREA SWEEPING" for total included line angles < 180∞    ;;
        ;;                                                          ;;
        ;;----------------------------------------------------------;;
        ((< 0 LTangle 180.0)        ; check total included line angle.

          ;; Increase the number of points on segment part 1
          ;; start with First pt. of segline: NIL
          ;;        add Last  pt. of segline: NIL
          (setq tmp1 (F:rb-GET-INCR incr segline1 NIL NIL)

                ;; calculate additional points for segment part
                tmp (F:rb-GET-AREA-SWEEP-PTS_<_180
                     pt0 pt1            ; first segment part
                     incr
                     LTangle
                     )
                )

          ;; Save each point with its ball angle and segment part in list
          (foreach pt (F:rb-ADD-SWEEP-PTS pt1 incr segline1 tmp1 tmp) ; join point lists
                   (setq Alist2 (cons (list pt ballang1 segline1) Alist2))
                   )

          ;; Increase the number of points on segment part 2
          ;; start with First pt. of segline: NIL
          ;;        add Last  pt. of segline: 'T
          (setq tmp2 (F:rb-GET-INCR incr segline2 NIL 'T)

                ;; calculate additional points for segment part
                tmp (F:rb-GET-AREA-SWEEP-PTS_<_180
                     pt0 pt2            ; second segment part
                     incr
                     LTangle
                     )
                )

          ;; Save each point with its ball angle and segment part in list
          (foreach pt (F:rb-ADD-SWEEP-PTS pt0 incr segline2 tmp2 tmp) ; join point lists
                   (setq Alist2 (cons (list pt ballang2 segline2) Alist2))
                   )
          )                             ; < 0 LTangle 180.0
        ;;----------------------------------------------------------;;
        ;;                                                          ;;
        ;; NO "AREA SWEEPING" if total included line angle = 180∞.  ;;
        ;; Line segments are colinear, the last point of `segline1' ;;
        ;; and the first point of the next segment part are equal,  ;;
        ;; so the first point of `segline2' will not be added       ;;
        ;; to list `Alist2'.                                        ;;
        ;;                                                          ;;
        ;;----------------------------------------------------------;;
        ((= LTangle 180.0)          ; check total included line angle.
          ;; Increase the number of points on segment part 1
          ;; start with First pt. of segline: NIL
          ;;        add Last  pt. of segline: 'T
          (setq tmp1 (F:rb-GET-INCR incr segline1 NIL 'T) )

          ;; Save each point with its ball angle and segment part in list
          (foreach pt tmp1 (setq Alist2 (cons (list pt ballang1 segline1) Alist2)) )

          ;; Increase the number of points on segment part 2
          ;; start with First pt. of segline: NIL
          ;;        add Last  pt. of segline: NIL
          (setq tmp2 (F:rb-GET-INCR incr segline2 NIL NIL ) )

          ;; Save each point with its ball angle and segment part in list
          (foreach pt tmp2 (setq Alist2 (cons (list pt ballang2 segline2) Alist2)) )
          )
        ;;----------------------------------------------------------;;
        ;;                                                          ;;
        ;; "AREA SWEEPING" for total included line angles > 180∞    ;;
        ;;                                                          ;;
        ;;----------------------------------------------------------;;
        ((and
           (>  LTangle 180.0)       ; check total included line angle.
           (<= LTangle 360.0)
           )

          ;; Increase the number of points on segment part 1
          ;; start with First pt. of segline: NIL
          ;;        add Last  pt. of segline: 'T
          (setq tmp1 (F:rb-GET-INCR incr segline1 NIL 'T) )

          ;; Save each point with its ball angle and segment part in list
          (foreach pt tmp1 (setq Alist2 (cons (list pt ballang1 segline1) Alist2)) )


          ;; Get the Number of Part Angles for total included Ball angle
          (setq PAngNum (F:rb-GET-PART-ANGLE-NUM_>180
                         LTangle ; Total Included line angle in Degrees
                         ))

          (if (>= PAngNum 2) ; do `area sweeping' for angle > 180 degrees
            (progn

              (setq ;; Total included ball angle
                    BTangle (F:rb-INC-ANG ballang1 ballang2)

                    ;; Part angle of total included ball angle
                    PAng (/ BTangle PAngNum)

                    cnt 0               ; Part angle counter
                    )

              ;; Calculate ball angles for corner points
              (while (< (setq cnt (1+ cnt)) PAngNum)

                ;; Get direction point `dp' for ball angle, `ballang1' is
                ;; the start angle for calculating the direction point
                ;; for the part ball angles.
                (if (= "LEFT" side)
                  (setq dp (polar pt0 (F:rb-AngleFix (- ballang1 (* cnt PAng))) 1.0 ))
                  (setq dp (polar pt0 (F:rb-AngleFix (+ ballang1 (* cnt PAng))) 1.0 ))
                  )

                ;; save corner point, ballangle & segment
                (setq Alist2 (cons
                              (list
                               pt0			; corner point
                               (angle pt0 dp) ; Ball angle for corner pt.
                               segline1 ; segment part of 1. border
                               )
                              Alist2
                              ))

                ;;(grdraw pt0 (polar pt0 (angle pt0 dp) (F:rb-ARROW-LEN)) 1)
                ;;(break (rtos LTangle 2 5))
                )

              )
            )

          ;; Increase the number of points on segment part 2
          ;; start with First pt. of segline: 'T
          ;;        add Last  pt. of segline: 'T
          (setq tmp2 (F:rb-GET-INCR incr segline2 'T 'T))

          ;; Save each point with its ball angle and segment part in list
          (foreach pt tmp2 (setq Alist2 (cons (list pt ballang2 segline2) Alist2)) )
          )                             ;_ > LTangle 180.0
        ('T
          (princ "\n *** CAUTION - Something unexpected happened at point ")
          (princ pt0)
          (princ " \n Total included line angle: ") (princ LTangle)
          (princ "\n Segment 1: ") (princ segline1)
          (princ "\n Segment 2: ") (princ segline2)
          (princ "\n")
          )
        )
      )
    )                                   ;_ end while Alist1

	(cond
    ;;--------------------------------------------
    ;; If 1. polyline is closed remove points
    ;; of overlapping start and end segment.
    ;;--------------------------------------------
    (Aclosed
      (setq tmp  nil
            tmp1 nil
            tmp2 nil
            )

      ;; Remove points of overlapping last segment
      (setq tmp (last (car Alist2)))    ; the last segment in list
      (while (equal (last (car Alist2)) tmp)
        (setq tmp1   (car Alist2)
              Alist2 (cdr Alist2)
              )
        )
      (setq Alist2 (reverse Alist2)) ; reverse the list to get the list start

      ;; Remove points of overlapping first segment
      (setq tmp (last (car Alist2)))    ; the first segment in list
      (while (equal (last (car Alist2)) tmp)
        (setq tmp2   (car Alist2)
              Alist2 (cdr Alist2)
              )
        )

      ;; Add the first point, `tmp1' or `tmp2'
      (cond
        ((< LTangle1 180) (if tmp1 (setq Alist2 (cons tmp1 Alist2))))
        ((= LTangle1 180) (if tmp2 (setq Alist2 (cons tmp2 Alist2))))
        ((> LTangle1 180) nil)
        ('T nil)
        )

      ;; Remove first point if it is double
      (setq tmp1 (car Alist2) tmp2 (cadr Alist2))
      (if (equal (car tmp1) (car tmp2) 1e-8)
				(setq Alist2 (cdr Alist2))
        )

      )
    ('T
      ;; Reverse the list to get the start point back
      (setq Alist2 (reverse Alist2))
      )
    )

	(princ "\r   Building ball angle list ... DONE")
	(princ "  (Time: ")
	(princ (- (F:rb-GET-TIME) time1))   ; elapsed time for building list
	(princ " sec.)\n")

	Alist2																; return list
  )
;;;
;;;-------------------------------------------------------------------
;;;
;;;                      MAIN APPLICATIONS
;;;
;;;-------------------------------------------------------------------
;;;
;; Check polyline for the first self intersecting segments.
(defun C:CHPL () (F:rb-CHPL 1))

;; Check 2 polylines for intersection.
(defun C:CHPL2 () (F:rb-CHPL 2))

(defun F:rb-CHPL ( mode / ENT ENT2 INT PLST PLST2)
  (cond
    ((= mode 1)
      (princ "\nCheck polyline for the first self intersecting segments.")
      (setq ent (F:rb-EntSelF
                 "\n Choose a Polyline: "
                 '((0 . "POLYLINE,LWPOLYLINE"))
                 )
            )
      (if ent
        (progn
          (setq plst (F:rb-GET-POLY-PTS (car ent)) )
          (princ "\n ..working\n")
          (setq int (car (F:rb-PL-ISECT plst)))
          (if (car int)
            (F:rb-GRDRAW-POINT int 2 "box")
            (princ "\n No self intersecting segments\n")
            )
          )
        )
      )
    ((= mode 2)
      (princ "\nCheck 2 polyline for intersection.")
      (setq ent (F:rb-EntSelF
                 "\n Choose 1. Polyline: "
                 '((0 . "POLYLINE,LWPOLYLINE"))
                 )
            )
      (if ent
        (progn
          (setq ent2 (F:rb-EntSelF
                      "\n Choose 2. Polyline: "
                      '((0 . "POLYLINE,LWPOLYLINE"))
                      )
                )

          (if ent2
            (progn
              (setq plst  (F:rb-GET-POLY-PTS (car ent))
                    plst2 (F:rb-GET-POLY-PTS (car ent2))
                    )
              (princ "\n ..working\n")
              (setq int (car (F:rb-PL-ISECT (append plst plst2))))
              (if (car int)
                (F:rb-GRDRAW-POINT int 2 "box")
                (princ "\n No intersection\n")
                )
              )
            )
          )
        )
      )

    )

  (if int
    int
    (princ)
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Angle measurement
;;
;; p1 +
;;    |
;;    |
;; p0 O----+ p2
;;
(defun C:IA ( / a1 basept e1 e2 osm )
  (setq osm (getvar "OSMODE"))
  (setvar "OSMODE" 1)
  (setq basept (getpoint "\nWinkel-Scheitelpunkt angeben:")) ; Dimang point 0
  (setvar "OSMODE" 512)
  (setq e1 (getpoint "\n1. Winkelendpunkt angeben:")) ; Dimang point 1
  (setq e2 (getpoint "\n2. Winkelendpunkt angeben:")) ; Dimang point 2
  (setvar "OSMODE" osm)
  (princ "\nMaﬂtext = ")
  (setq a1 (F:rb-RTD (F:rb-INC-ANG (angle basept e1)(angle basept e2))))
  (princ a1)
  (princ ", ")
  (princ (- 360.0 a1))
  (princ)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Show intersection of a line with circle radius line when line is
;; crossing circle in 1 or 2 points, does not work with tangent lines.
(defun C:ILC ( / line p1 p2 p3 rad circ tmp)
  (redraw)
  (setq line (car (entsel "Select line: "))
        circ (car (entsel "Select circle: "))
        )
  (if (and line circ)
    (progn
      (setq p1  (cdr (assoc 10 (entget line)))
            p2  (cdr (assoc 11 (entget line)))
            rad (cdr (assoc 40 (entget circ)))
            p3  (cdr (assoc 10 (entget circ)))
            )

      (if (setq tmp (F:rb-INTERS-LC (list p1 p2) p3 rad nil))
        (F:rb-GRDRAW-POINT (nth 2 tmp) 2 "X")
        )
      )
    )

  (if tmp
    (nth 2 tmp)
    (progn
      (princ "\nNo intersection!")
      (princ)
      )
    )
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Count polyline vertices
(defun C:VERTCNT ( / cnt c_cmd ename etype vlist)
  (redraw)
  (setq ename (car (entsel "\n Select a Polyline: ")))
  (princ "\n")
  (if ename
    (progn
      (setq **CONT* nil
            **ALT*  nil
            )
      (setq c_cmd (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (setq etype (cdr (assoc 0 (entget ename))))
      (setq vlist (F:rb-GET-POLY-PTS ename))
      (if (equal (car vlist)(last vlist)) (setq vlist (cdr vlist)))
      (foreach p vlist (F:rb-GRDRAW-POINT p 1 "X"))
      (prompt (strcat "\r There are " (itoa (length vlist)) " vertices in this " etype "."))
      (if *DEBUG*
        (progn
          (setq *VX* vlist)
          (princ "\n Vertices are stored in global variable *VX*")
          )
        )
      (setvar "CMDECHO" c_cmd)
      )
    )
  (princ)
  )
;;;
;;;-------------------------------------------------------------------
;;;
;; Ball visible
(defun C:ROLLIN () (F:rb-ROLLIN "ROLLIN") (princ))

;; Ball hidden
(defun C:ROLLIN2 () (F:rb-ROLLIN "ROLLIN2") (princ))

;; Revision: 16.12.2005
;;
(defun F:rb-ROLLIN ( program / ACLOSED ALENGTH ALIST2 APICKPT
                             BCLOSED BLENGTH BLIST COL0 COL1
                             ENT1 ENT1S ENT2 ENT2S INCR INCRB LAY0 LAY1 LAY2
                             LAY3 LAY4 LAY5 LAYPREFIX MLENGTH SANG SIDE TLEN
                             TMP TOTAL TO_DO limit
                             ;; local functions
                             #DEBUG #FIB
                             )

  ;; for debugging
  (defun #DEBUG ()
    (cond (*DEBUG*
            (princ " - break at number: ")
            (princ (fix (getvar "USERR5")))
            (if (= to_do (fix (getvar "USERR5"))) (setq *break* 'T) )
            (break (itoa to_do))
            )))

  ;; calculate fibonacci number
  (defun #FIB (n / #FIB-ITER)
    (defun #FIB-ITER (a b count)
      (if (= count 0)
        b
        (#FIB-ITER (+ a b) a (- count 1))
        )
      )
    (#FIB-ITER 1 1 n)
    )

  (setq lay0 (getvar "CLAYER")
        col0 (getvar "CECOLOR")
        )

  (F:rb-PRINT-CHAR "=" 75) (princ "#")

  (F:rb-SETUP)

  (setvar "USERS1" program)

  ;; color for all
  (setq col1 (getvar "CECOLOR"))
  (cond
    ((wcmatch (getvar "CECOLOR") "*LAYER*") (setq col1   2) )
    ((wcmatch (getvar "CECOLOR") "*BLOCK*") (setq col1 255) )
    ('T (setq col1 (atoi (getvar "CECOLOR"))) )
    )

  (setq ;; Layer names
        layprefix (strcat "RB-" (itoa col1) "-")
        lay1 (strcat layprefix "Centerline") ; Centerline between boundaries
        lay2 (strcat layprefix "Circles"   ) ; Circles
        lay3 (strcat layprefix "Radius"    ) ; Lines from circle center to boundary hit points
        lay4 (strcat layprefix "Error_Centerline-corrected") ; Defect centerline corrected
        lay5 (strcat layprefix "Error_Centerline-NOT-corrected") ; Centerline defect and NOT corrected
        **COL1* (itoa col1)    ; global copy of main color (for debug)
        )

  ;; Select the polyline boundaries
  (setq tmp (F:rb-SELECT-BORDERS))

  (if tmp
    (progn
      ;; Boundary selection
      (setq ent1s   (nth 0 tmp)         ; 1. boundary
            ent2s   (nth 1 tmp)         ; 2. boundary
            ent1    (car  ent1s)        ; object name 1. boundary
            ent2    (car  ent2s)        ; object name 2. boundary
            Apickpt (cadr ent1s)        ; pick point on 1. boundary
            )

      ;; Get length of boundaries
      (command "_area" "_o" ent1)
      (setq Alength (getvar "PERIMETER"))
      (princ (strcat "\n  The total length of 1. border is: " (rtos (getvar "PERIMETER") 2 4)))
      (command "_area" "_o" ent2)
      (setq Blength (getvar "PERIMETER"))
      (princ (strcat "\n  The total length of 2. border is: " (rtos (getvar "PERIMETER") 2 4)))

      (setq Mlength (max Alength Blength))

      ;; GET POLYLINE SEGMENT POINTS OF 1. & 2. BOUNDARY and check for double points
      (setq **ALIST* (F:rb-REMOVE-DOUBLE-PTS (F:rb-GET-POLY-PTS ent1 )))
      (setq BLIST    (F:rb-REMOVE-DOUBLE-PTS (F:rb-GET-POLY-PTS ent2 )))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                         ;;
      ;; TRY TO GET THE POINT LISTS RUN INTO THE SAME DIRECTION  ;;
      ;; AND TO GET THEIR START POINTS AS CLOSE AS POSSIBLE.     ;;
      ;;                                                         ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq ;; Find out whether Boundary polylines are open or closed
            Aclosed (F:rb-PL-CLOSED? ent1)
            Bclosed (F:rb-PL-CLOSED? ent2)

            ;; Change the order in the lists to bring the startpoints
            ;; of the lists close to each other.
            tmp (F:rb-ALIGN-LISTS **ALIST* BLIST Apickpt Aclosed Bclosed)
            **ALIST* (car tmp)
            BLIST    (cadr tmp)
            Aclosed  (caddr tmp)
            )

      ;; Mark start pt. and direction of the 1. & 2. border
      (F:rb-MARK-POINT (car **ALIST*)(cadr **ALIST*) 1)
      (F:rb-MARK-POINT (car BLIST)(cadr BLIST) 2)

      ;;--------------------------------------------------------------
      ;; GET VALUES FROM USER
      ;;--------------------------------------------------------------
      ;; GET INCREMENT ALONG THE 1. BOUNDARY
      (while (or (not incr)(< incr 0))
        (setq incr
                (F:rb-UDIST
                 1
                 ""
                 "\n What is the increment along the boundary to check for ball fit?"
                 (if (>= (getvar "USERR1") 0)
                   (getvar "USERR1")
                   Alength
                   )
                 (car **ALIST*)         ; point, `nil' for none
                 )))

      (setvar "USERR1" incr)    ; save increment along the 1. boundary
      (princ (strcat "   Increment: " (rtos incr) "\n"))

      ;;--------------------------------------------------------------
      ;; CALCULATE BALL INCREASE FACTOR
      ;; My observations:
      ;; A small increment does not always automatically mean
      ;; a better precision, sometimes a larger value can
      ;; calculate better results. A larger value can be
      ;; also sometimes slower than a smaller value.
      ;;--------------------------------------------------------------
      (setq tmp  -1
            limit 9
            )
      (while (or (< tmp 0.1) (> tmp limit))
        (setq tmp
                (F:rb-UREAL
                 1
                 ""
                 " Ball increase factor, 0.1 - 9.0 ....... [0.5/1/2/3/4/5/6/7/8/9]"
                 (if (<= 0.1 (getvar "USERR4") limit)
                   (getvar "USERR4")
                   (- limit 1)
                   ))))

      (setvar "USERR4" tmp)          ; save ball increase speed factor

      (princ (strcat "   Increase factor: " (rtos tmp 2 2)))

      ;; Calculate final increment to increase size of ball.
      (setq incrb (* tmp (/ (#FIB (fix tmp)) 100.0)))
      ;;--------------------------------------------------------------

      ;; <SET> number of <P>art <A>ngles
      (F:rb-SETPA)

      ;; Revision: 29.11.2005
      ;; Choose side where to draw centerline
      (setq tmp (F:rb-GET-SIDE
                 (car  **ALIST*)
                 (cadr **ALIST*)
                 (car  BLIST)
                 (cadr BLIST)
                 "Show side to draw ball/centerline with <M-left-klick>: "
                 )
            ;; side point
            ;;sidePt (car tmp)

            ;; angle of side point from 1. boundary
            sang (cadr tmp)

            ;; side where to draw the ball, LEFT or RIGHT
            side (car (caddr tmp))      ; string, "LEFT" or "RIGHT"
            ;;sideN (last (caddr tmp))    ; number, 1 or -1
            )

      (princ side)
      (princ " side")

      ;;--------------------------------------------------------------
      (redraw ent1 4)                   ; remove line highlight
      (redraw ent2 4)
      (redraw)

      (if (or (not incr) (< incr 0) (not side)) ; check user input
        (princ "\n Wrong input!")
        (progn

          ;; Mark start pt. and direction of the 1. & 2. border
          (F:rb-MARK-POINT (car **ALIST*)(cadr **ALIST*) 1)
          (F:rb-MARK-POINT (car   BLIST) (cadr   BLIST)  2)

          ;; Mark the drawing side
          (F:rb-GRDRAW-ARROW (car **ALIST*)
                             sang
                             (* (F:rb-ARROW-LEN) 0.75) ; arrow length
                             4          ; arrow color
                             )

          (F:rb-PRINT-CHAR "-" 75)

          (F:rb-START-TIMER "\n=> Start time..") ; save process start time

          ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ;; Divide each segment into 2 parts, so we have later always the
          ;; segment connection point between two points when we process
          ;; the first part of the first segment separately. Then we also
          ;; have always the total included line angle of the two segments
          ;; and can calculate it.
          ;;
          ;;
          ;; | 1. part | 2. part | 1. part | 2. part |
          ;; O---------+---------O---------+---------O--------+-----
          ;; |<-   1. segment  ->|<-  2. segment   ->|<- 3. segment  -> ...
          ;;                     |
          ;;                     |
          ;;              connection point
          ;;
          (setq **ALIST* (F:rb-DIVIDE-SEGMENTS **ALIST*))
          ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


          ;; Make point pairs out of polyline points
          ;; '(<pt1> <pt2> <pt3> ...)  => '((<pt1> <pt2>) (<pt2> <pt3>) ...)
          ;; You now have the boundary lines in lists `**ALIST*' & `BLIST'.
          (setq **ALIST* (F:rb-MAKE-PT-PAIRS **ALIST*) ; global list
                BLIST    (F:rb-MAKE-PT-PAIRS BLIST)
                )

          ;; Split BLIST into sublists to increase performance
          ;; speed, each sublist contains a certain number of
          ;; line segments.
          (setq **BLIST-SPLIT* (F:rb-SPLIT-LIST 20 BLIST) ; global list
                BLIST          nil
                )

          ;; number of point pairs in list **ALIST*, 1. boundary
          (setvar "USERI1" (length **ALIST*))

          ;; number of point pairs sublists in list BLIST, 2. boundary
          (setvar "USERI2" (length **BLIST-SPLIT*))

          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;                                                          ;;
          ;; CALCULATE THE BALL ANGLE FOR EACH POINT OF FIRST BORDER  ;;
          ;;                                                          ;;
          ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (setq Alist2 (F:rb-BUILD-BALLANGLE-LIST **ALIST* Aclosed incr side))


          ;;OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO;;
          ;;                                                        ;;
          ;;                                                        ;;
          ;;               START OF DRAWING CIRCLES                 ;;
          ;;                                                        ;;
          ;;                                                        ;;
          ;;OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO;;
          (setvar "CECOLOR" (itoa col1)) ; set circle color

          (setq total (length Alist2) ; `total' used for progress report
                to_do total             ; running ball number
                total (itoa total)
                tlen  (strlen total)
                )

          (while Alist2 ; `Alist2' = list with lists of '(<point> <ball angle> <segment>)

            ;; .......................................................
            ;; Progress report
            (princ (strcat "\r   To do: "
                           (F:rb-STR-LPADCHAR (itoa (setq to_do (1- to_do))) tlen " ")
                           " of " total " | "
                           "Center points: "
                           (F:rb-STR-LPADCHAR (itoa (getvar "USERI5")) tlen " " )
                           ))
            ;;(#DEBUG)
            (princ "\r  ")
            ;; .......................................................

            ;; Draw ball/circle from point `int1' at angle `ballang1'
            (F:rb-DRAW-BALL
             (caar   Alist2) ; point on 1. border, start of ball, `int1'
             (cadar  Alist2)            ; ball angle, `ballang1'
             (caddar Alist2)            ; line segment, `segline1'
             incrb                      ; ball increase
             Mlength                    ; maximum ball increase
             col1                       ; ball color
             )

            (setq Alist2 (cdr Alist2)) ; remove point `int1' from main list
            )                           ;_ end while

          ;; Finally delete the circle from screen
          (if (= (type **CENT*) 'ENAME) (entdel **CENT*))

          (F:rb-SET-LAYER lay0 col0 'T) ; set original layer & color
          ;;OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO;;
          ;;                                                        ;;
          ;;                 END OF DRAWING CIRCLES                 ;;
          ;;                                                        ;;
          ;;OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO;;

          ;; Clear the command line
          (F:rb-CLEAR-CMDLINE 80)

          ;; Restore view
          (F:rb-VIEWSAVE 2)

          ;; Print final informations
          (princ (strcat "\r   To do: "
                         (F:rb-STR-LPADCHAR (itoa to_do) tlen " ")
                         " of " total ", "
                         "Center points: "
                         (F:rb-STR-LPADCHAR (itoa (getvar "USERI5")) tlen " ")
                         ))

          (if (> **CORRECTION-ERRORS* 0)
            (princ (strcat "\n ** " (itoa **CORRECTION-ERRORS*)
                           " intersection point(s) could not be corrected."
                           )))
          (F:rb-STOP-TIMER "\n=> Elapsed time" nil) ; print process time

          ;; Draw CENTER LINE and ask whether to draw Circles and/or Circle radius lines
          (F:rb-DRAW-RESULTS)
          )                             ; progn
        )                               ; if (or (not incr)...)
      )                                 ; progn
    )                                   ; if tmp

  (F:rb-RESET "undoEnd")                ; reset variables
  )
;;;
;;;-------------------------------------------------------------------
;;;
(princ "\n Rolling_ball.lsp v7.02\n Commands: \n")
(princ "\n CHPL    - check a polyline for the first self intersecting segments.")
(princ "\n CHPL2   - check 2 polylines for the first intersection.")
(princ "\n VERTCNT - counts and marks the vertices in a single polyline.")
(princ "\n ROLLIN  - ball visible")
(princ "\n ROLLIN2 - ball hidden, faster")
(princ)
;;; Rolling_ball.lsp ends here

;;; Local Variables:
;;; ispell-local-dictionary: "american"
;;; End:
