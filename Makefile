STACK_BUILD_DIR=.stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/tutoring-backend-exe

# sudo apt install sendemail for mail support

all:
	mkdir -p release
	# build frontend
	cd frontend && elm-make Main.elm --output main.js
	cp -r frontend/images/ release/
	cp frontend/index.html release/
	cp frontend/main.js    release/
	cp frontend/bulma.css  release/
	cp frontend/styles.css release/
	# build backend
	cd backend && stack build
	cp backend/$(STACK_BUILD_DIR)/tutoring-backend-exe release/tutoring-backend-exe
	# build docker container (later)
run:
	cd release && ./tutoring-backend-exe

migrate:
	cd release && ./tutoring-backend-exe migrate

test-data:
	cd release && ./tutoring-backend-exe test-data

drop-data:
	sqlite3 release/database.db "DROP TABLE news"

dump-sqlite:
	sqlite3 release/database.db "SELECT * FROM news"