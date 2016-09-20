## How To
Load Chirp through Quicklisp or ASDF:

    (ql:quickload :chirp)

To use twitter's API, you need to authorize an account. By default this happens through the PIN method, though others are available as well. Retrieve your [twitter application](https://apps.twitter.com/)'s api key and secret and invoke the following function:

    (chirp:initiate-authentication :api-key "<app api key>" :api-secret "<app api secret>")

You may use the following keys for testing purposes: API-KEY: `D1pMCK17gI10bQ6orBPS0w` API-SECRET: `BfkvKNRRMoBPkEtDYAAOPW4s2G9U8Z7u3KAf0dBUA`. These are for the CL-CHIRP twitter application. You should not use these for anything other than the testing of Chirp, as it may pose a security risk.

If the first OAuth step is successful, it should return an URL that you have to visit. This will then prompt you to authorize the application and present a PIN. Copy this pin and complete the authentication process:

    (chirp:complete-authentication "<pin>")

If the function returns successfully, you are ready to use the twitter API:

    (chirp:account/verify-credentials)

In the case that you do not want to repeat the authentication process, you can save and later set the `*oauth-api-key*`, `*oauth-api-secret*`, `*oauth-access-token*` and `*oauth-access-secret*` variables manually. That's all the information it takes to authenticate over twitter. Make sure to keep these tokens secret.

    (chirp:statuses/update "Hooray, I successfully used the Chirp Common Lisp library to tweet!")

There are functions to check for tweet length, available languages, access level, rate limits, and so on as well.

    (chirp:compute-status-length "Wowsers, URL shortening sure is a thing! https://github.com/Shinmera/chirp.git")
    (chirp:valid-language-p "en")
    (chirp:access-level)

Chirp also provides access to twitter's streaming API:

    (chirp:stream/user #'(lambda (message) (when message (format T "~&amp;STREAM: ~a~%" message)) T))

Do note that Chirp does not concern itself with threading. As such, processing stream objects in the background is up to you.


Using the various convenience methods, a simple bot can be assembled with relative ease:

    (chirp:map-timeline :mentions #'(lambda (status) (chirp:reply status "Chirp chirp!")))

Though the timelines are heavily rate-limited. For a more immediate response, the streaming API should be used:

    (chirp:start-stream
      :user #'(lambda (message)
                (when (and (typep message 'chirp:status) (chirp:direct-mention-p message))
                  (chirp:reply message "Chirp chirp!"))
                T))

One thing to note is that twitter XML entity encodes certain things like status texts. I frankly don't know why it does that since it's a JSON api. Chirp does not automatically decode these entities, as the twitter entities (like hashtags, urls, etc) contain position markers that depend on the encoded string. Decoding it screws over these positions. However, Chirp offers a couple of functions to make handling of entities or decoding easier:

    (chirp:xml-decode (chirp:text status))
    (chirp:text-with-expanded-urls status)
    (chirp:text-with-markup status)
    (chirp:replace-entity :urls #'expanded-url)

Especially the last two can be very useful for preparing the text for a web-interface.
Chirp's symbols are separated into three packages (and unified in `CHIRP`) so that you may selectively `USE` what you need.

 * `CHIRP-API` contains all the direct Twitter API call mapping functions.
 * `CHIRP-EXTRA` contains the various helper functions that make dealing with the API easier.
 * `CHIRP-OBJECTS` contains all accessor and class symbols. You probably want to `USE` this package if you work with the objects a lot.

