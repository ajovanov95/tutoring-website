FROM ubuntu:16.04
LABEL Aleksandar Jovanov (aleksandar.jovanov.1995@gmail.com)

# Install dependencies
RUN apt update
RUN apt install -y sendemail 
RUN apt install -y -f libpq-dev

# RUN apt install -y curl

# Haskell runtime dependencies?
# Copy code and compile with stack in the image?
# Compile code approach
# RUN curl -sSL https://get.haskellstack.org/ | sh
# RUN mkdir -p /home/aleksandar/tutoring-website/static
# RUN mkdir -p /home/aleksandar/backend-code
# COPY backend/* /home/aleksandar/backend-code
# RUN  cd /home/aleksandar/backend-code/ && stack build && cp .stack-work/**/build/tutoring-backend-exe /home/aleksandar/tutoring-website/tutoring-backend-exe

# copy files frontend
COPY release/static/images/aleksandar.jpg /home/aleksandar/tutoring-website/static/images/aleksandar.jpg
COPY release/static/bulma.css /home/aleksandar/tutoring-website/static/bulma.css
COPY release/static/styles.css /home/aleksandar/tutoring-website/static/styles.css
COPY release/static/main.js /home/aleksandar/tutoring-website/static/main.js
COPY release/static/index.html /home/aleksandar/tutoring-website/static/index.html
COPY release/static/admin.html /home/aleksandar/tutoring-website/static/admin.html
COPY release/static/admin.js /home/aleksandar/tutoring-website/static/admin.js

# copy files backend
COPY release/tutoring-backend-exe /home/aleksandar/tutoring-website/tutoring-backend-exe
COPY release/docker-run.sh /home/aleksandar/tutoring-website/docker-run.sh

RUN chmod +x /home/aleksandar/tutoring-website/docker-run.sh

# Run the server when starting this image
ENTRYPOINT ["/bin/sh", "/home/aleksandar/tutoring-website/docker-run.sh"]