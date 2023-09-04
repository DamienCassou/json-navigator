ELPA_DEPENDENCIES=package-lint assess buttercup hierarchy

ELPA_ARCHIVES=melpa gnu

TEST_ERT_FILES=test/json-navigator-test.el
LINT_CHECKDOC_FILES=json-navigator.el test/json-navigator-test.el
LINT_PACKAGE_LINT_FILES=json-navigator.el
LINT_COMPILE_FILES=json-navigator.el test/json-navigator-test.el

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://github.com/DamienCassou/makel/raw/v0.8.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
