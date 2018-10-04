#!/usr/bin/env bash

sed -i "s#<service-uri>#${SERVICE_URI}#" /etc/nginx/nginx.conf

# Replace start.sh with nginx
exec nginx -g "daemon off;"
