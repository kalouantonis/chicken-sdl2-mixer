(module
 sdl2-mixer
 (open-audio
  close-audio
  load-music
  play-music
  halt-music
  pause-music
  resume-music
  rewind-music
  music-finished
  music-type
  music-volume
  music-playing?
  load-sample
  play-sample
  pause-channel
  resume-channel
  halt-channel
  channel-finished
  channel-playing?
  channel-paused?)

 (import chicken scheme foreign)
 (use sdl2-mixer-lolevel)

 (foreign-declare "#include <SDL2/SDL.h>")

 (define audio-opened #f)
 (define initialized #f)

 (define (sdl-mixer-condition #!optional reason)
   (signal (make-composite-condition
            (make-property-condition 'exn  'message (or reason (mix-get-error)))
            (make-property-condition 'sdl)
            (make-property-condition 'mixer))))

 (define (open-audio #!key
                     (sampling-rate 44100)
                     (sample-format AUDIO_S16SYS)
                     (channels 2)
                     (chunk-size 1024))
   (when audio-opened
         (mix-close-audio))
   (unless initialized
           ((foreign-lambda void "SDL_Init" unsigned-integer32)
	    (foreign-value "SDL_INIT_AUDIO" int))
           (mix-init (bitwise-ior MIX_INIT_MP3 MIX_INIT_OGG MIX_INIT_FLAC))
           (set! initialized #t))
   (mix-open-audio sampling-rate sample-format channels chunk-size))

 (define (close-audio)
   (when (and initialized audio-opened)
         (set! audio-opened #f)
         (mix-close-audio)))

 (define (load-music filename)
   (let ((m (mix-load-mus filename)))
     (unless m (sdl-mixer-condition))
     (set-finalizer! m mix-free-music)
     m))

 (define (play-music music #!key
                     (repeat 0)
                     (fadein #f)
                     (volume #f))
   (let ((m (if (string? music)
                (load-music music)
                music))
         (r (if (eq? repeat #:forever) -1 repeat)))
     (when volume (mix-volume-music volume))
     (if (= -1
            (cond (fadein (mix-fadein-music m r fadein))
                  (else (mix-play-music m r))))
         (sdl-mixer-condition)
         m)))

 (define (halt-music #!key fadeout)
   (if fadeout
       (mix-fadeout-music fadeout)
       (mix-halt-music)))

 (define pause-music mix-pause-music)
 (define resume-music mix-resume-music)
 (define rewind-music mix-rewind-music)

 (define (music-finished handler)
   (unless (procedure? handler)
           (error "music-finished: Not a procedure " handler))
   (set-music-finished-cb handler))

 (define (music-type m)
   (let ((t (mix-get-music-type m)))
     (cond
      ((equal? t MUS_CMD) 'user-specific)
      ((equal? t MUS_WAV) 'wav)
      ((equal? t MUS_MP3) 'mp3)
      ((equal? t MUS_MOD) 'mod)
      ((equal? t MUS_MID) 'midi)
      ((equal? t MUS_OGG) 'ogg)
      (else 'unkown))))

 (define (music-volume #!optional new)
   (mix-volume-music (or new -1)))

 (define music-playing? mix-playing-music)

 (define (load-sample filename)
   (let ((s (mix-loadWAV filename)))
     (unless (mix-chunk-pointer s) (sdl-mixer-condition))
     (set-finalizer! s mix-free-chunk)
     s))

 (define (play-sample sample
                      #!key
                      (channel -1)
                      (repeat #f)
                      (fadein #f)
                      (duration #f))
   (let* ((r (or repeat 0))
          (c (cond ((and duration fadein)
                    (mix-fadein-channel-timed channel sample r fadein duration))
                   (duration
                    (mix-play-channel-timed channel sample r duration))
                   (fadein
                    (mix-fadein-channel channel sample r fadein))
                   (else
                    (mix-play-channel channel sample r)))))
     (if (= -1 c) (sdl-mixer-condition) c)))

 (define (pause-channel #!optional (channel -1))
   (mix-pause channel))

 (define (resume-channel #!optional (chan -1))
   (mix-resume chan))

 (define (halt-channel #!optional (channel -1) #!key fadeout)
   (if fadeout
       (mix-fadeout-channel channel fadeout)
       (mix-halt-channel channel)))

 (define (channel-finished handler)
   (unless (procedure? handler)
           (error "channel-finished: Not a procedure: " handler))
   (set-mix-channel-finished-cb handler))

 (define channel-playing? mix-playing)
 (define channel-paused? mix-paused) )
