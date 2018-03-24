# Issues
# 3. Various open shift stuff

FROM ubuntu:18.04
LABEL Aleksandar Jovanov (aleksandar.jovanov.1995@gmail.com)

# Install dependencies
RUN apt update
RUN apt install -y sendemail sqlite3

# Haskell runtime dependencies?
# Copy code and compile with stack in the image?
# Compile code approach
# RUN curl -sSL https://get.haskellstack.org/ | sh

RUN mkdir -p /home/aleksandar/tutoring-website/static

# copy files frontend
COPY release/static/images/aleksandar.jpg /home/aleksandar/tutoring-website/static/images/aleksandar.jpg
COPY release/static/bulma.css /home/aleksandar/tutoring-website/static/bulma.css
COPY release/static/styles.css /home/aleksandar/tutoring-website/static/styles.css
COPY release/static/main.js /home/aleksandar/tutoring-website/static/main.js
COPY release/static/index.html /home/aleksandar/tutoring-website/static/index.html

# copy files backend
COPY release/database.db /home/aleksandar/tutoring-website/database.db
COPY release/tutoring-backend-exe /home/aleksandar/tutoring-website/tutoring-backend-exe
COPY release/docker-run.sh /home/aleksandar/tutoring-website/docker-run.sh

RUN chmod +x /home/aleksandar/tutoring-website/docker-run.sh

# Run the server when starting this image
ENTRYPOINT ["/bin/sh", "/home/aleksandar/tutoring-website/docker-run.sh"]

# EXPOSE port 8000
EXPOSE 8000