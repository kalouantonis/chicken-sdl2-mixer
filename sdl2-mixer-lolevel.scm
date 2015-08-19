(module sdl2-mixer-lolevel
        *

        (import chicken scheme foreign)
        (use lolevel (srfi 4 18) posix)
        (foreign-declare "#include <SDL2/SDL_mixer.h>")

        (define-syntax --sdl-flags
          (lambda (e r c)
            `(,(r 'begin)
              ,@(append-map (lambda (str)
                              (let* ((sym (string->symbol str))
                                     (psym (string->symbol (string-append "-" str))))
                                `((,(r 'define-foreign-variable) ,psym unsigned-integer ,str)
                                  (,(r 'define) ,sym ,psym))))
                            (cdr e)))))

        (--sdl-flags
         "MIX_INIT_FLAC"
         "MIX_INIT_MOD"
         "MIX_INIT_MP3"
         "MIX_INIT_OGG")

        (define mix-init (foreign-lambda int "Mix_Init" integer32))
        (define mix-quit (foreign-lambda void "Mix_Quit"))


        (--sdl-flags
         "AUDIO_U8"
         "AUDIO_S8"
         "AUDIO_U16LSB"
         "AUDIO_S16LSB"
         "AUDIO_U16MSB"
         "AUDIO_S16MSB"
         "AUDIO_U16"
         "AUDIO_S16"
         "AUDIO_U16SYS"
         "AUDIO_S16SYS"
         "MIX_DEFAULT_FORMAT")

        (define mix-open-audio (foreign-lambda integer32 "Mix_OpenAudio" integer32 unsigned-short integer32 integer32))
        (define mix-close-audio (foreign-lambda void "Mix_CloseAudio"))
        (define mix-set-error! (foreign-lambda* void ((c-string str)) "Mix_SetError(\"%s\", str);"))
        (define mix-get-error (foreign-lambda c-string "Mix_GetError"))

        (define-record query-spec opened frequency format channels)
        (define %query_spec (foreign-lambda integer32 "Mix_QuerySpec"
                                            (c-pointer integer32) (c-pointer unsigned-short) (c-pointer integer32)))
        (define (mix-query-spec)
          (let ((opened 0))
            (let-location ((freq integer32)
                           (format unsigned-short)
                           (channels integer32))
                          (set! opened (%query_spec #$freq #$format #$channels))
                          (unless (<= 0 opened)
                                  (error "mix-query-spec:" (mix-get-error)))
                          (make-query-spec opened freq format channels))))


        (define mix-get-num-chunk-decoders
          (foreign-lambda integer32 "Mix_GetNumChunkDecoders"))

        (define mix-get-chunk-decoder
          (foreign-lambda c-string "Mix_GetChunkDecoder" integer32))

        (define-record mix-chunk pointer)
        (define-record-printer (mix-chunk c out)
          (print "#<mix-chunk " (mix-chunk-pointer c) ">"))

        (define-foreign-type Mix_Chunk (c-pointer (struct "Mix_Chunk"))
          mix-chunk-pointer
          make-mix-chunk)

        (define mix-chunk-allocated
          (foreign-lambda* unsigned-integer32
                           ((Mix_Chunk c))
                           "C_return(c->allocated);"))

        (define mix-chunk-volume
          (foreign-lambda* integer
                           ((Mix_Chunk c))
                           "C_return(c->volume);"))

        (define mix-chunk-sample-length
          (foreign-lambda* unsigned-integer32
                           ((Mix_Chunk c))
                           "C_return(c->alen);"))

        (define mix-chunk-abuf
          (foreign-lambda* (c-pointer unsigned-char)
                           ((Mix_Chunk c))
                           "C_return(c->abuf);"))

        (define (mix-chunk-sample-data c)
          (let* ((l (mix-chunk-sample-length c))
                 (b (make-u8vector l))
                 (p (mix-chunk-abuf c)))
            (move-memory! p b l)
            b))

        (define mix-loadWAV
          (foreign-lambda Mix_Chunk "Mix_LoadWAV" nonnull-c-string))

        #;
        (define mix-loadWAV-RW
        (foreign-lambda Mix_Chunk
        "Mix_LoadWAV_RW"
        SDL_RWops
        integer))

        (define mix-quickload-WAV
          (foreign-lambda Mix_Chunk "Mix_QuickLoad_WAV" (c-pointer unsigned-char)))

        (define mix-quickload-RAW
          (foreign-lambda Mix_Chunk "Mix_QuickLoad_RAW" (c-pointer unsigned-char) unsigned-integer32))

        (define mix-volume-chunk
          (foreign-lambda integer "Mix_VolumeChunk" Mix_Chunk integer))

        (define mix-free-chunk
          (foreign-lambda void "Mix_FreeChunk" Mix_Chunk))

        (define mix-allocate-channels
          (foreign-lambda integer "Mix_AllocateChannels" integer))

        (define mix-volume
          (foreign-lambda integer "Mix_Volume" integer integer))

        (define mix-play-channel
          (foreign-lambda integer "Mix_PlayChannel" integer Mix_Chunk integer))

        (define mix-play-channel-timed
          (foreign-lambda integer "Mix_PlayChannelTimed" integer Mix_Chunk integer integer))

        (define mix-fadein-channel
          (foreign-lambda integer "Mix_FadeInChannel" integer Mix_Chunk integer integer))

        (define mix-fadein-channel-timed
          (foreign-lambda integer "Mix_FadeInChannelTimed" integer Mix_Chunk integer integer integer))

        (define mix-pause
          (foreign-lambda void "Mix_Pause" integer))

        (define mix-resume
          (foreign-lambda void "Mix_Resume" integer))

        (define mix-halt-channel
          (foreign-lambda void "Mix_HaltChannel" integer))

        (define mix-expire-channel
          (foreign-lambda void "Mix_ExpireChannel" integer integer))

        (define mix-fadeout-channel
          (foreign-lambda integer "Mix_FadeOutChannel" integer integer))


#>
int channel_callback_fd = -1;
static void channel_finished (int channel){
	if (channel_callback_fd >= 0) {
                char chan[3];
                int ignore;
        	snprintf(chan, 3, "%d\n", channel);
                ignore = write(channel_callback_fd, chan, 2);
	}
}
<#
        (define-external channel_callback_fd int -1)

	(define mix-channel-finished-scheme-cb #f)

        (define (make-waiter-thread handler)
          (let-values (((in out) (create-pipe)))
            (set! channel_callback_fd out)
            (let ((p (open-input-file* in)))
              (let loop ()
                (thread-wait-for-i/o! in #:input)
                (let ((v (read p)))
                  (unless (eof-object? v)
                          (handler v)
                          (loop))
                  (close-input-port p))))))

        (define (set-mix-channel-finished-cb handler)
          (when (eq? handler mix-channel-finished-scheme-cb)
                (foreign-code "close(channel_callback_fd);"))
          (set! mix-channel-finished-scheme-cb handler)
          (foreign-code "Mix_ChannelFinished(channel_finished);")
          (thread-start! (make-thread (cut make-waiter-thread handler))))

        (define mix-playing
          (foreign-lambda integer "Mix_Playing" integer))

        (define mix-paused
          (foreign-lambda integer "Mix_Paused" integer))

        (--sdl-flags
         "MIX_NO_FADING"
         "MIX_FADING_OUT"
         "MIX_FADING_IN")

        (define-foreign-type Mix_Fading integer)

        (define mix-fading-channel
          (foreign-lambda Mix_Fading "Mix_FadingChannel" integer))

        (define mix-get-chunk
          (foreign-lambda Mix_Chunk "Mix_GetChunk" integer))

        (define mix-reserve-channels
          (foreign-lambda integer "Mix_ReserveChannels" integer))

        (define mix-group-channel
          (foreign-lambda integer "Mix_GroupChannel" integer integer))

        (define mix-group-channels
          (foreign-lambda integer "Mix_GroupChannels" integer integer integer))

        (define mix-group-count
          (foreign-lambda integer "Mix_GroupCount" integer))

        (define mix-group-available
          (foreign-lambda integer "Mix_GroupAvailable" integer))

        (define mix-group-oldest
          (foreign-lambda integer "Mix_GroupOldest" integer))

        (define mix-group-newer
          (foreign-lambda integer "Mix_GroupNewer" integer))

        (define mix-fadeout-group
          (foreign-lambda integer "Mix_FadeOutGroup" integer integer))

        (define mix-halt-group
          (foreign-lambda integer "Mix_HaltGroup" integer))

        (define mix-get-num-music-decoders
          (foreign-lambda integer "Mix_GetNumMusicDecoders"))

        (define mix-get-music-decoder
          (foreign-lambda c-string "Mix_GetMusicDecoder" integer))

        (define-foreign-type Mix_Music c-pointer)

        (define mix-load-mus
          (foreign-lambda Mix_Music "Mix_LoadMUS" nonnull-c-string))

        (define mix-free-music
          (foreign-lambda void "Mix_FreeMusic" Mix_Music))

        (define mix-play-music
          (foreign-lambda integer "Mix_PlayMusic" Mix_Music integer))

        (define mix-fadein-music
          (foreign-lambda integer "Mix_FadeInMusic" Mix_Music integer integer))

        (define mix-fadein-music-pos
          (foreign-lambda integer "Mix_FadeInMusicPos" Mix_Music integer integer double))

#>
int music_callback_fd = -1;
static void music_finished (){
	if (music_callback_fd >= 0) {
                int ignore = write(music_callback_fd, "1\n", 2);
	}
}
<#
        (define-external music_callback_fd int -1)

	(define music-finished-scheme-cb #f)

        (define (make-music-waiter-thread handler)
          (let-values (((in out) (create-pipe)))
            (set! music_callback_fd out)
            (let ((p (open-input-file* in)))
              (let loop ()
                (thread-wait-for-i/o! in #:input)
                (let ((v (read p)))
                  (unless (eof-object? v)
                          (handler)
                          (loop))
                  (close-input-port p))))))

        (define (set-music-finished-cb handler)
          (when (eq? handler music-finished-scheme-cb)
                (foreign-code "close(music_callback_fd);"))
          (set! music-finished-scheme-cb handler)
          (foreign-code "Mix_HookMusicFinished(music_finished);")
          (thread-start! (make-thread (cut make-music-waiter-thread handler))))

        (define mix-volume-music
          (foreign-lambda integer "Mix_VolumeMusic" integer))

        (define mix-pause-music
          (foreign-lambda void "Mix_PauseMusic"))

        (define mix-resume-music
          (foreign-lambda void "Mix_ResumeMusic"))

        (define mix-rewind-music
          (foreign-lambda void "Mix_RewindMusic"))

        (define mix-set-music-position
          (foreign-lambda integer "Mix_SetMusicPosition" double))

        (define mix-set-music-cmd
          (foreign-lambda integer "Mix_SetMusicCMD" c-string))

        (define mix-halt-music
          (foreign-lambda integer "Mix_HaltMusic"))

        (define mix-fadeout-music
          (foreign-lambda integer "Mix_FadeOutMusic" integer))

        ;; Mix_Music_Type
        (--sdl-flags
         "MUS_NONE"
         "MUS_CMD"
         "MUS_WAV"
         "MUS_MOD"
         "MUS_MID"
         "MUS_OGG"
         "MUS_MP3")

        (define mix-get-music-type
          (foreign-lambda integer "Mix_GetMusicType" Mix_Music))

        (define mix-playing-music
          (foreign-lambda integer "Mix_PlayingMusic"))

        (define mix-paused-music
          (foreign-lambda integer "Mix_PausedMusic"))

        (define mix-fading-music
          (foreign-lambda Mix_Fading "Mix_FadingMusic"))

        ;; XXX Mix_GetMusicHookData unimplemented

        ;; XXX No Effect procedures implemented so far
        )
