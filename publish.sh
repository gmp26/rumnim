#!/bin/bash

cd ~/clojure/rumnim
lein clean
lein cljsbuild once min
rsync -av resources/public/* gmp26@pan.maths.org:/www/nrich/html/drips
