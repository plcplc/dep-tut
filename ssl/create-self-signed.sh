openssl req -x509 -out localhost.pem -keyout localhost.key \
  -newkey rsa:2048 -nodes -sha256   -subj '/CN=localhost' \
  -days 3650 \
  -addext subjectAltName=DNS:localhost,IP:127.0.0.1

