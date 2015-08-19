(use sdl-mixer)

(open-audio)

;; Procedure to be called when a channel finishes playing.
(channel-finished 
  (lambda (c)
    (printf "Channel ~a finished~%" c)))

(let ([s (load-sample "sample.wav")])
  (play-sample s))

(close-audio)
