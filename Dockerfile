FROM fpco/stack-build
COPY . /app
WORKDIR /app
# Cache deps
RUN stack build --only-dependencies
RUN stack build --copy-bins /app/
RUN ls

FROM alpine:latest  
RUN MKDIR /app
COPY --from=0 /app/tg-bot /app/bin
COPY .env /app/env
CMD /app/bin
