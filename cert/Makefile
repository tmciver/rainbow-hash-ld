# X.509 Client Certificate Generation with Subject Alternative Names (SAN)
# Variables - customize these as needed
CERT_NAME ?= client
DOMAIN ?= localhost
EMAIL ?= user@example.com
IP ?= 127.0.0.1
VALIDITY_DAYS ?= 365
KEY_SIZE ?= 2048
COUNTRY ?= US
STATE ?= CA
CITY ?= San Francisco
ORG ?= Example Corp
OU ?= IT Department

# File names
PRIVATE_KEY = $(CERT_NAME).key
CSR_FILE = $(CERT_NAME).csr
CERT_FILE = $(CERT_NAME).crt
CONFIG_FILE = $(CERT_NAME).conf

# Default target
.PHONY: all
all: $(CERT_FILE)

# Generate private key
$(PRIVATE_KEY):
	openssl genpkey -algorithm RSA -out $(PRIVATE_KEY) # -pkcs12 -aes256
	chmod 600 $(PRIVATE_KEY)

# Check that configuration file exists
$(CONFIG_FILE):
	@if [ ! -f $(CONFIG_FILE) ]; then \
		echo "Error: Configuration file $(CONFIG_FILE) not found."; \
		echo "Please copy $(CERT_NAME).conf.sample to $(CONFIG_FILE) and customize as needed."; \
		echo "Run 'make config' to create a default configuration file."; \
		exit 1; \
	fi

# Generate certificate signing request (CSR)
$(CSR_FILE): $(CONFIG_FILE) $(PRIVATE_KEY)
	openssl req -new -key $(PRIVATE_KEY) -out $(CSR_FILE) -config $(CONFIG_FILE)

# Generate self-signed certificate with SAN
$(CERT_FILE): $(CONFIG_FILE) $(PRIVATE_KEY)
	openssl req -x509 -new -key $(PRIVATE_KEY) -out $(CERT_FILE) \
		-days $(VALIDITY_DAYS) -config $(CONFIG_FILE) -extensions v3_cert

# Create configuration file from sample
.PHONY: config
config:
	@if [ -f $(CONFIG_FILE) ]; then \
		echo "Configuration file $(CONFIG_FILE) already exists."; \
		echo "Remove it first if you want to recreate it from the sample."; \
	else \
		cp $(CERT_NAME).conf.sample $(CONFIG_FILE); \
		echo "Created $(CONFIG_FILE) from sample template."; \
		echo "Please edit $(CONFIG_FILE) to customize certificate details."; \
	fi

# Generate certificate without password protection (for testing)
.PHONY: cert-nopass
cert-nopass: $(CONFIG_FILE)
	openssl genpkey -algorithm RSA -out $(CERT_NAME)-nopass.key -pkcs8
	chmod 600 $(CERT_NAME)-nopass.key
	openssl req -x509 -new -key $(CERT_NAME)-nopass.key -out $(CERT_NAME)-nopass.crt \
		-days $(VALIDITY_DAYS) -config $(CONFIG_FILE) -extensions v3_cert

# Verify certificate
.PHONY: verify
verify: $(CERT_FILE)
	@echo "=== Certificate Information ==="
	openssl x509 -in $(CERT_FILE) -text -noout
	@echo
	@echo "=== Certificate Validation ==="
	openssl x509 -in $(CERT_FILE) -noout -checkend 86400 && echo "Certificate is valid for the next 24 hours" || echo "Certificate expires within 24 hours"

# Show certificate details
.PHONY: info
info: $(CERT_FILE)
	@echo "=== Subject ==="
	openssl x509 -in $(CERT_FILE) -noout -subject
	@echo "=== Issuer ==="
	openssl x509 -in $(CERT_FILE) -noout -issuer
	@echo "=== Validity ==="
	openssl x509 -in $(CERT_FILE) -noout -dates
	@echo "=== Subject Alternative Names ==="
	openssl x509 -in $(CERT_FILE) -noout -ext subjectAltName

# Show certificate modulus and exponent
.PHONY: modulus
modulus: $(CERT_FILE)
	@echo "=== Certificate Public Key Modulus ==="
	openssl x509 -in $(CERT_FILE) -noout -modulus
	@echo "=== Certificate Public Key Exponent ==="
	openssl x509 -in $(CERT_FILE) -noout -text | grep -A 1 "Exponent:"

# Test certificate and key pair
.PHONY: test
test: $(CERT_FILE) $(PRIVATE_KEY)
	@echo "Testing certificate and private key pair..."
	@cert_md5=$$(openssl x509 -noout -modulus -in $(CERT_FILE) | openssl md5); \
	key_md5=$$(openssl rsa -noout -modulus -in $(PRIVATE_KEY) | openssl md5); \
	if [ "$$cert_md5" = "$$key_md5" ]; then \
		echo "✓ Certificate and private key match"; \
	else \
		echo "✗ Certificate and private key do NOT match"; \
		exit 1; \
	fi

# Create PKCS12 bundle
.PHONY: pkcs12
pkcs12: $(CERT_FILE) $(PRIVATE_KEY)
	@echo "Creating PKCS12 bundle..."
	openssl pkcs12 -export -out $(CERT_NAME).p12 -inkey $(PRIVATE_KEY) -in $(CERT_FILE) -name "$(CERT_NAME)"

# Convert to different formats
.PHONY: convert
convert: $(CERT_FILE) $(PRIVATE_KEY)
	@echo "Converting to different formats..."
	openssl pkcs12 -export -out $(CERT_NAME).p12 -inkey $(PRIVATE_KEY) -in $(CERT_FILE) -name "$(CERT_NAME)"
	openssl x509 -in $(CERT_FILE) -out $(CERT_NAME).pem -outform PEM

# Clean up generated files (preserve sample config)
.PHONY: clean
clean:
	rm -f *.key *.crt *.csr *.p12 *.pem
	@if [ -f $(CONFIG_FILE) ] && [ -f $(CERT_NAME).conf.sample ]; then \
		echo "Keeping $(CONFIG_FILE) (use 'make clean-config' to remove it)"; \
	fi

# Clean configuration file
.PHONY: clean-config
clean-config:
	rm -f $(CONFIG_FILE)

# Clean everything including backup files
.PHONY: distclean
distclean: clean
	rm -f *~ *.bak

# Show help
.PHONY: help
help:
	@echo "X.509 Client Certificate Generation Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  all         - Generate private key and self-signed certificate (default)"
	@echo "  config      - Create configuration file from sample template"
	@echo "  cert-nopass - Generate certificate without password protection"
	@echo "  verify      - Verify and display certificate information"
	@echo "  info        - Show certificate subject, issuer, validity, and SAN"
	@echo "  modulus     - Show certificate public key modulus and exponent"
	@echo "  test        - Test that certificate and private key match"
	@echo "  pkcs12      - Create PKCS#12 bundle from certificate and key"
	@echo "  convert     - Convert to PKCS#12 and PEM formats"
	@echo "  clean       - Remove generated certificate files (keep config)"
	@echo "  clean-config- Remove configuration file"
	@echo "  distclean   - Remove all generated and backup files"
	@echo "  help        - Show this help message"
	@echo ""
	@echo "Configurable variables:"
	@echo "  CERT_NAME     - Certificate name prefix (default: client)"
	@echo "  DOMAIN        - Domain name for CN and SAN (default: localhost)"
	@echo "  EMAIL         - Email address for certificate (default: user@example.com)"
	@echo "  IP            - IP address for SAN (default: 127.0.0.1)"
	@echo "  VALIDITY_DAYS - Certificate validity in days (default: 365)"
	@echo "  KEY_SIZE      - RSA key size in bits (default: 2048)"
	@echo "  COUNTRY       - Country code (default: US)"
	@echo "  STATE         - State/Province (default: CA)"
	@echo "  CITY          - City/Locality (default: San Francisco)"
	@echo "  ORG           - Organization (default: Example Corp)"
	@echo "  OU            - Organizational Unit (default: IT Department)"
	@echo ""
	@echo "Getting started:"
	@echo "  1. Run 'make config' to create configuration file from template"
	@echo "  2. Edit $(CERT_NAME).conf to customize certificate details"
	@echo "  3. Run 'make all' to generate certificate and private key"
	@echo ""
	@echo "Example usage:"
	@echo "  make config"
	@echo "  make CERT_NAME=mycert"
	@echo "  make cert-nopass"
	@echo "  make verify info"
