STACK_BUILD_DIR=.stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/tutoring-backend-exe
POSTGRES_HOST=localhost

all:
	mkdir -p release/static/
	# build frontend
	cd frontend && elm-make Main.elm --output main.js
	cd frontend && elm-make Admin.elm --output admin.js
	# copy files frontend
	cp -r frontend/images/ release/static/
	cp frontend/index.html release/static/
	cp frontend/main.js    release/static/
	cp frontend/bulma.css  release/static/
	cp frontend/styles.css release/static/
	cp frontend/admin.html release/static/
	cp frontend/admin.js   release/static/
	# build backend
	cd backend && stack build
	cp backend/$(STACK_BUILD_DIR)/tutoring-backend-exe release/tutoring-backend-exe
	cp backend/docker-run.sh release/
run:
	export PORT=8000
	export DATABASE_URL=postgresql://postgres:postgres@localhost:5432/test-db
	cd release && ./tutoring-backend-exe

docker-build:
	cd release && ./tutoring-backend-exe migrate

	docker build -t aleksandar-tutoring-website .

docker-run:
	# port_to:port_from, volume_to:directory_from
	docker run -d -p 80:8000 \
	-e PORT=8000 \
	-e DATABASE_URL=postgresql://postgres:postgres@$(POSTGRES_HOST):5432/test-db \
	--name tutoring-backend aleksandar-tutoring-website \

# Actions related to database
migrate:
	cd release && ./tutoring-backend-exe migrate

test-data:
	cd release && ./tutoring-backend-exe test-data
