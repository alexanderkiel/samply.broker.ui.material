FROM node:10.12.0-jessie as build

COPY . /build/

WORKDIR /build
RUN npm install --production
RUN npm run test
RUN npm run build

FROM nginx:1.15.5

RUN rm /usr/share/nginx/html/*
COPY --from=build /build/dist/main-*.js /usr/share/nginx/html/
COPY --from=build /build/dist/index.html /usr/share/nginx/html/

COPY docker/nginx.conf /etc/nginx/
COPY docker/flags.js /
COPY docker/flags.sed /
COPY docker/start.sh /
RUN chmod +x /start.sh

CMD ["/start.sh"]
