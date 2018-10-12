#!/usr/bin/env bash

sed -i "s#<search-store-root>#${SEARCH_STORE_ROOT}#" /etc/nginx/nginx.conf
sed -i "s#<websocket-timeout-seconds>#${WEBSOCKET_TIMEOUT_SECONDS:-60}#" /etc/nginx/nginx.conf
sed -i "s#<mdr-root>#${MDR_ROOT}#" /flags.js
sed -i "s#<mdr-namespace>#${MDR_NAMESPACE}#" /flags.js
sed -i -f /flags.sed /usr/share/nginx/html/index.html

# Replace start.sh with nginx
exec nginx -g "daemon off;"
