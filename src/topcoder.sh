#!/bin/bash

cd `dirname $0`

curl\
 --remote-name \
 --remote-header-name\
 http://www.topcoder.com/contest/arena/ContestAppletProd.jnlp

JAVAWS="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/javaws"

if [ ! -x "$JAVAWS" ]; then
  # Fallback to PATH's javaws if the plugin isn't found
  JAVAWS=javaws
fi

"$JAVAWS" ContestAppletProd.jnlp
