# Makefile
.PHONY: help \
	install \
	test-changed \
	test-full \
	test-watch \
	test-watch-changed \
	coverage-html \
	ci-container \
	ci-act \
	ci-local \
	lint \
	clean \
	build \
	upload-pypi-test \
	upload-pypi \
	release

help:
	@echo "Available commands:"
	@echo "  install        Install project dependencies"
	@echo "  agreement      Test src-test one-on-one agreements"
	@echo "  agreement-coverage"
	@echo "  test-changed   Run tests which affected by source code change (fast)"
	@echo "  test-full      Run full tests with coverage"
	@echo "  test-watch     Run tests in watch mode (auto-rerun on changes)"
	@echo "  test-watch-changed Run changed tests in watch mode"
	@echo "  coverage-html  Generate HTML coverage report"
	@echo "  ci-container   Run CI with containers (Apptainer -> Docker fallback)"
	@echo "  ci-act         Run GitHub Actions locally with Act and Apptainer"
	@echo "  ci-local       Run local CI emulator (Python-based)"
	@echo "  lint           Run linting and formatting"
	@echo "  clean          Remove cache files"
	@echo "  build          Build package for distribution"
	@echo "  upload-pypi-test Upload to Test PyPI"
	@echo "  upload-pypi    Upload to PyPI"
	@echo "  release        Clean, build, and upload to PyPI"

install:
	pip install -e ".[dev]"
	pre-commit install

agreement:
	python tests/custom/test_src_test_agreement.py

agreement-coverage:
	@result=$$(python tests/custom/test_src_test_agreement.py --shell); \
	data=$$(echo "$$result" | tail -1); \
	complete=$$(echo $$data | cut -d: -f2); \
	total=$$(echo $$data | cut -d: -f1); \
	ratio=$$(echo $$data | cut -d: -f3); \
	echo "$$complete/$$total (= $$ratio%)"

COVERAGE_DATA_FILE = tests/reports/.coverage

test-watch:
	@echo "Running tests in watch mode..."
	pytest-watch --runner \
		"pytest tests/ -v \
			--cov=src/semantic_search_engine \
			--cov-report=html:tests/reports/watch-full/htmlcov \
	 		--cov-report=json:tests/reports/watch-full/coverage.json \
			--cov-report=term-missing"

test-watch-changed:
	@echo "Running changed tests in watch mode..."
	pytest-watch --runner \
		"pytest --testmon tests/ -v \
			--cov=src/semantic_search_engine \
			--cov-report=html:tests/reports/watch-changed/htmlcovc \
	 		--cov-report=json:tests/reports/watch-changed/coverage.json \
			--cov-report=term-missing"

test-full:
	@echo "Running tests with coverage..."
	COVERAGE_FILE=$(COVERAGE_DATA_FILE) pytest tests/ -v \
		--cov=src/semantic_search_engine \
		--cov-report=html:tests/reports/full/htmlcov \
		--cov-report=json:tests/reports/full/coverage.json \
		--cov-report=term-missing

test-changed:
	@echo "Running tests affected by code changes..."
	COVERAGE_FILE=$(COVERAGE_DATA_FILE) pytest --testmon tests/ -v \
		--cov=src/semantic_search_engine \
		--cov-report=html:tests/reports/changed/htmlcov \
		--cov-report=json:tests/reports/changed/coverage.json \
		--cov-report=term-missing

coverage-html:
	@echo "Generating HTML coverage report..."
	coverage html --data-file=$(COVERAGE_DATA_FILE) -d tests/reports/htmlcov

ci-container:
	@echo "🏗️ Running CI on HPC with containers (direct)..."
	./tests/github_actions/run_ci_container.sh

ci-act:
	@echo "⚡ Running GitHub Actions locally with Singularity..."
	./tests/github_actions/run_ci_act_and_container.sh

ci-local:
	@echo "🚀 Running local CI emulator (Python-based)..."
	./tests/github_actions/run_ci_local.sh

lint:
	@echo "Running linting and formatting..."
	ruff check src/ tests/
	ruff format src/ tests/

clean:
	@echo "Cleaning cache files..."
	find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete
	rm -rf .mypy_cache .pytest_cache tests/reports/htmlcov tests/reports/coverage.json tests/github/local_ci_report.json build/ dist/ *.egg-info/ 2>/dev/null || true
	chmod -R +w .ruff_cache 2>/dev/null || true
	rm -rf .ruff_cache 2>/dev/null || true

build:
	@echo "Building package..."
	python -m build

upload-pypi-test:
	@echo "Uploading to Test PyPI..."
	python -m twine upload --repository testpypi dist/*

upload-pypi:
	@echo "Uploading to PyPI..."
	python -m twine upload dist/*

release: build upload-pypi clean
	@echo "Package released to PyPI!"
