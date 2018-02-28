all:
	mkdir -p release
	# build backend
	cd backend && stack build
	cp backend/.stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/tutoring-backend-exe/tutoring-backend-exe release/tutoring-backend-exe
	# build frontend
	cd frontend && elm-make Main.elm --output main.js
	cp -r frontend/images/ release/
	cp frontend/index.html release/
	cp frontend/main.js    release/
	cp frontend/styles.css release/
	# build docker container (later)
run:
	cd release && ./tutoring-backend-exe