# Next chord prediction

Neural network trained on some songs (my advice: from one genre)
will interactively propose you a further chord.


### How to use

```
stack build
stack exec music-fun-datagen < song1
stack exec music-fun-datagen < song2
stack exec music-fun-datagen < song3
...
stack exec music-fun-executor
c
[("c#",0.9772864594483655),("g#",0.8260367735753724)]
C
[("g",0.933959594730716),("d#",0.7903379275067666),("c",0.6343168385193068),("f#",0.6012203674328802)]
c7
[("g",0.933959594730716),("d#",0.7903379275067666),("c",0.6343168385193068),("f#",0.6012203674328802)]
c#
[("g",0.933959594730716),("d#",0.7903379275067666),("c",0.6343168385193068),("f#",0.6012203674328802)]
f$
[("g",0.9464612559894285),("e",0.8232864504074877)]
```

Utility script is provided, which will generate network based on songs from given directory

```
./learn.sh songs/disco
# generates network.nn
```

Have fun! :)
