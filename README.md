# tau

*The simple XMPP bot.*

Put code in `Bot.hs`.

```
cabal run HOSTNAME XMPPID XMPPPASS XMPPNICK XMPPROOM
```

## Installation
`tau` depends on features not in the current `pontarius-xmpp 0.4.0.1`
package, hence the git submodule. To install:

1. Clone the repo + cd into the folder
2. `git submodule init`
3. `git submodule update`
4. Bump the `Version` in `pontarius-xmpp.cabal` in the `pontarius-xmpp` folder to `0.4.0.2`
5. `cabal sandbox init`
6. `cabal sandbox add-source ./pontarius-xmpp`
7. `cabal install --only-dependencies` (you may need to add the `--extra-include-dirs` and `--extra-lib-dirs` flags
   to point to where you have the [ICU](http://site.icu-project.org/) library installed)
8. `cabal build`

And you're good to go!

## HipChat Instructions

- HOSTNAME: `chat.hipchat.com`
- XMPPID: `1234_1234@chat.hipchat.com/bot`
- XMPPPASS: `password`
- XMPPNICK: `My Name`
- XMPPROOM: `1234_words`
