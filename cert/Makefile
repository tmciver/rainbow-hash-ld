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

# Create OpenSSL configuration file with SAN
$(CONFIG_FILE):
	@echo "Creating OpenSSL configuration file..."
	@echo "[req]" > $(CONFIG_FILE)
	@echo "default_bits = $(KEY_SIZE)" >> $(CONFIG_FILE)
	@echo "prompt = no" >> $(CONFIG_FILE)
	@echo "distinguished_name = req_distinguished_name" >> $(CONFIG_FILE)
	@echo "req_extensions = v3_req" >> $(CONFIG_FILE)
	@echo "" >> $(CONFIG_FILE)
	@echo "[req_distinguished_name]" >> $(CONFIG_FILE)
	@echo "C = $(COUNTRY)" >> $(CONFIG_FILE)
	@echo "ST = $(STATE)" >> $(CONFIG_FILE)
	@echo "L = $(CITY)" >> $(CONFIG_FILE)
	@echo "O = $(ORG)" >> $(CONFIG_FILE)
	@echo "OU = $(OU)" >> $(CONFIG_FILE)
	@echo "CN = $(DOMAIN)" >> $(CONFIG_FILE)
	@echo "emailAddress = $(EMAIL)" >> $(CONFIG_FILE)
	@echo "" >> $(CONFIG_FILE)
	@echo "[v3_req]" >> $(CONFIG_FILE)
	@echo "basicConstraints = CA:FALSE" >> $(CONFIG_FILE)
	@echo "keyUsage = critical, digitalSignature, keyEncipherment, keyAgreement" >> $(CONFIG_FILE)
	@echo "extendedKeyUsage = clientAuth" >> $(CONFIG_FILE)
	@echo "subjectAltName = @alt_names" >> $(CONFIG_FILE)
	@echo "" >> $(CONFIG_FILE)
	@echo "[v3_cert]" >> $(CONFIG_FILE)
	@echo "basicConstraints = CA:FALSE" >> $(CONFIG_FILE)
	@echo "keyUsage = critical, digitalSignature, keyEncipherment, keyAgreement" >> $(CONFIG_FILE)
	@echo "extendedKeyUsage = clientAuth" >> $(CONFIG_FILE)
	@echo "subjectAltName = @alt_names" >> $(CONFIG_FILE)
	@echo "" >> $(CONFIG_FILE)
	@echo "[alt_names]" >> $(CONFIG_FILE)
	@echo "DNS.1 = $(DOMAIN)" >> $(CONFIG_FILE)
	@echo "DNS.2 = www.$(DOMAIN)" >> $(CONFIG_FILE)
	@echo "IP.1 = $(IP)" >> $(CONFIG_FILE)
	@echo "email.1 = $(EMAIL)" >> $(CONFIG_FILE)
	@echo "URI.1 = https://$(DOMAIN)/" >> $(CONFIG_FILE)

# Generate certificate signing request (CSR)
$(CSR_FILE): $(PRIVATE_KEY) $(CONFIG_FILE)
	openssl req -new -key $(PRIVATE_KEY) -out $(CSR_FILE) -config $(CONFIG_FILE)

# Generate self-signed certificate with SAN
$(CERT_FILE): $(PRIVATE_KEY) $(CONFIG_FILE)
	openssl req -x509 -new -key $(PRIVATE_KEY) -out $(CERT_FILE) \
		-days $(VALIDITY_DAYS) -config $(CONFIG_FILE) -extensions v3_cert

# Generate certificate without password protection (for testing)
.PHONY: cert-nopass
cert-nopass:
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

# Convert to different formats
.PHONY: convert
convert: $(CERT_FILE) $(PRIVATE_KEY)
	@echo "Converting to different formats..."
	openssl pkcs12 -export -out $(CERT_NAME).p12 -inkey $(PRIVATE_KEY) -in $(CERT_FILE) -name "$(CERT_NAME)"
	openssl x509 -in $(CERT_FILE) -out $(CERT_NAME).pem -outform PEM

# Clean up generated files
.PHONY: clean
clean:
	rm -f *.key *.crt *.csr *.conf *.p12 *.pem

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
	@echo "  cert-nopass - Generate certificate without password protection"
	@echo "  verify      - Verify and display certificate information"
	@echo "  info        - Show certificate subject, issuer, validity, and SAN"
	@echo "  test        - Test that certificate and private key match"
	@echo "  convert     - Convert to PKCS#12 and PEM formats"
	@echo "  clean       - Remove generated certificate files"
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
	@echo "Example usage:"
	@echo "  make CERT_NAME=mycert DOMAIN=example.com EMAIL=admin@example.com"
	@echo "  make cert-nopass DOMAIN=testserver.local"
	@echo "  make verify info"
