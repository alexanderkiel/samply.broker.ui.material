#!/usr/bin/env bash

sed -i "s#<service-uri>#${SERVICE_URI}#" /etc/nginx/nginx.conf
sed -i "s#<mdr-root>#${MDR_ROOT}#" /flags.js
sed -i -f /flags.sed /usr/share/nginx/html/index.html

# Replace start.sh with nginx
exec nginx -g "daemon off;"
