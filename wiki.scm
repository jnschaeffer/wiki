(import (chicken irregex)
	(chicken random)
	(chicken tcp)
	html-parser
	http-client
	spiffy
	srfi-1
	srfi-28
	unicode-char-sets
	uri-common
	utf8
	utf8-srfi-13
	utf8-srfi-14)

;; gets a random element from a list, by making it into a vector (of course).
(define (random-element lst)
  (let* ((vec (list->vector lst))
	 (len (vector-length vec)))
    (vector-ref vec (pseudo-random-integer len))))

;; makes paragraphs from HTML. does this by chunking all text found within paragraph tags and
;; calling those paragraphs. all other tags are stripped out.
(define (make-paragraphs p)
  (let* ((parse
	  (make-html-parser
	   ;; in these functions, seed is defined as a (in-paragraph? . accum) pair.
	   ;; the general idea is that set! isn't really preferred here, so if we
	   ;; can avoid it we may as well.

	   ;; (#t accum) if it's a paragraph
	   'start: (lambda (tag attrs seed virtual?)
		     (cons (or (car seed) (eq? tag 'p)) (cdr seed)))

	   ;; (#f (cons "" accum)) if it's a paragraph
	   'end: (lambda (tag attrs parent-seed seed virtual?)
		   (let* ((state (car seed)))
		     (if (and state (eq? tag 'p))
			 (cons #f (cons "" (cdr seed)))
			 (cons state (cdr seed)))))

	   'text: (lambda (text seed)
		    (let* ((state (car seed))
			   (cur-p (cadr seed))
			   (prev-ps (cddr seed))
			   (next (if state
				     (string-append cur-p text)
				     cur-p)))
		      (cons state (cons next prev-ps)))))))
    (cddr (parse (cons #f '("")) p))))

;; makes sentences from a paragraph. does this by using a gross regex to find good sentence
;; endings, including those with wiki references at the end, then tokenizing the paragraph by
;; whitespace characters and folding it into lists of sentence lists. the output is a reversed list
;; of reversed sentences, but that's fine because we only use this to get a random sentence later.
(define (make-sentences p)
  (let* ((words (string-tokenize p (char-set-complement char-set:white-space)))
	 (end-ir (irregex "(([^\\.]|^)\\.|\\?|!)(\\[[a-zA-Z0-9\\? ]+\\])*$"))
	 (citation-ir (irregex "\\[[a-zA-Z0-9\\?]+\\]"))
	 (builder
	  (lambda (word sentences)
	    (let* ((is-end (irregex-search end-ir word))
		   (new-word (if is-end
				 (irregex-replace citation-ir word "")
				 word))
		   (cur-sentence (car sentences))
		   (new-sentence (cons new-word cur-sentence)))
	      (if is-end
		  (cons '() (cons new-sentence (cdr sentences)))
		  (cons new-sentence (cdr sentences))))))
	 (sentences (fold builder '(()) words)))
    (if (not (null? (car sentences))) ;; incomplete sentence at the end of the paragraph
	sentences
	(cdr sentences))))

;; joins the reversed-list sentence words together by reversing them and joining with " ".
(define (sentence->string sentence)
  (string-join (reverse sentence) " "))

(define (random-sentence-from-paragraphs cur-sentence paragraphs)
  (if (> (string-length cur-sentence) 0)
      cur-sentence
      (let* ((paragraph (random-element paragraphs))
	    (sentences (make-sentences paragraph))
	    (sentence (if (not (null? sentences))
			  (random-element sentences)
			  '()))
	    (sentence-str (sentence->string sentence)))
	(random-sentence-from-paragraphs sentence-str paragraphs))))

;; the fun function. splits HTML input from a port into paragraphs, then gets a random sentence,
;; then returns that as a string. some paragraphs are currently empty, so this may return an empty
;; string.
(define (random-sentence-from-html port)
  (let* ((paragraphs (make-paragraphs port))
	 (sentence-str (random-sentence-from-paragraphs "" paragraphs)))
    sentence-str))

;; gets a random sentence from a random article on wikipedia. if add-newline? is #t, also adds a
;; newline character to the result.
(define (random-wikipedia-sentence add-newline?)
  (let ((sentence (call-with-input-request
		   "https://en.wikipedia.org/wiki/Special:Random"
		   #f
		   random-sentence-from-html)))
    (if add-newline?
	(format "~a~n" sentence)
	sentence)))

;; handles a server request. this gets a random sentence with newline from wikipedia, sets the
;; content-type header appropriately, and sends the sentence back to the user.
(define (handle-request continue)
  (let ((request (current-request))
	(sentence (random-wikipedia-sentence #t)))
    (with-headers
     '((content-type #(text/plain ((charset . utf-8)))))
     (lambda ()
       (send-response status: 'ok body: sentence)
       (continue)))))

;; runs the random wiki server.
(define (run-server)
  (tcp-buffer-size 2048)
  (vhost-map `(("localhost" . ,handle-request)))
  (access-log (current-output-port))
  (display "running server on port 8080\n")
  (start-server port: 8080))
