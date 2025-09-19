# DILIGAF Ecosystem

The DILIGAF programming language comes with a comprehensive ecosystem of libraries designed specifically for security professionals, penetration testers, and ethical hackers.

## 🚀 **Core Libraries**

### **1. DILIGAF Core (`libs/core/diligaf-core.diligaf`)**
Essential utilities and base functionality for all DILIGAF programs.

**Features:**
- System information and platform detection
- File operations and directory management
- Process management and system monitoring
- Environment variable handling
- Temporary file management
- Logging and debugging utilities
- Cross-platform compatibility

**Usage:**
```diligaf
stackit core

# System information
info = get_system_info()
print("Platform:", info["platform"])
print("Architecture:", info["architecture"])

# File operations
write_file("test.txt", "Hello DILIGAF")
content = read_file("test.txt")
print("Content:", content)

# Process management
pid = get_process_id()
print("Current PID:", pid)

# Logging
info("This is an info message")
warning("This is a warning")
error("This is an error")
```

### **2. Networking (`libs/networking/diligaf-net.diligaf`)**
Comprehensive networking and protocol libraries.

**Features:**
- Socket programming (TCP/UDP)
- HTTP/HTTPS client with custom headers
- DNS resolution and reverse lookups
- FTP client for file transfers
- SSH client for remote execution
- Port scanning and network discovery
- Packet sniffing and analysis
- Network interface management

**Usage:**
```diligaf
stackit net

# HTTP requests
response = http_get("https://example.com")
print("Status:", response["status_code"])
print("Content:", response["content"])

# DNS queries
ips = dns_query("example.com", "A")
print("IP addresses:", ips)

# Port scanning
open_ports = scan_common_ports("192.168.1.1")
print("Open ports:", open_ports)

# Packet sniffing
start_sniffing("eth0", "tcp port 80")
packets = get_packets()
analysis = analyze_packets()
print("Packet analysis:", analysis)
```

### **3. Cryptography (`libs/crypto/diligaf-crypto.diligaf`)**
Advanced cryptographic functions and security tools.

**Features:**
- Hash functions (MD5, SHA1, SHA256, SHA512, BLAKE2)
- Symmetric encryption (AES, DES, 3DES, RC4, XOR, Caesar, ROT13)
- Asymmetric encryption (RSA, DSA, ECDSA, Ed25519)
- Message Authentication Codes (HMAC, CMAC, Poly1305)
- Password hashing (PBKDF2, Scrypt, Argon2, bcrypt)
- Digital signatures and verification
- Steganography (LSB, DCT, DWT)
- Key generation and management

**Usage:**
```diligaf
stackit crypto

# Hash functions
hash_value = hash_data("Hello World", "sha256")
print("SHA256:", hash_value)

# AES encryption
key = generate_key("aes", 256)
encrypted = encrypt_aes("Secret message", key)
decrypted = decrypt_aes(encrypted, key)
print("Decrypted:", decrypted)

# RSA key pair
keypair = generate_rsa_keypair(2048)
private_key = keypair["private_key"]
public_key = keypair["public_key"]

# Digital signature
signature = sign_rsa("Message to sign", private_key)
is_valid = verify_rsa("Message to sign", signature, public_key)
print("Signature valid:", is_valid)

# Password hashing
hashed = hash_password("mypassword", "pbkdf2")
is_correct = verify_password("mypassword", hashed)
print("Password correct:", is_correct)
```

### **4. Exploit Development (`libs/exploit/diligaf-exploit.diligaf`)**
Payload generation and exploit development tools.

**Features:**
- Shellcode generation (x86/x64, Linux/Windows)
- Payload generation (Python, PowerShell, PHP, JavaScript)
- Buffer overflow exploits and pattern generation
- SQL injection payloads (UNION, Boolean, Time-based, Error-based)
- XSS payloads (Reflected, Stored, DOM-based)
- Command injection payloads
- Payload obfuscation and encoding
- Exploit automation and testing

**Usage:**
```diligaf
stackit exploit

# Generate shellcode
shellcode = generate_shellcode("x86", "linux", "reverse_shell")
print("Shellcode:", shellcode.hex())

# Generate Python payload
python_payload = generate_python_payload("reverse_shell", "192.168.1.100", 4444)
print("Python payload:", python_payload)

# SQL injection
union_payload = generate_union_payload(3, "users", "1=1")
print("UNION payload:", union_payload)

# XSS payload
xss_payload = generate_reflected_xss("alert")
print("XSS payload:", xss_payload)

# Buffer overflow
pattern = generate_pattern(1000)
offset = find_offset(pattern, "0x41414141")
print("Offset:", offset)

# Test exploits
result = test_sql_injection("http://target.com/login", "username", union_payload)
print("SQL injection result:", result)
```

## 📦 **Package Manager**

### **DILIGAF Package Manager (`diligaf-pm.diligaf`)**
Install, manage, and update DILIGAF libraries and packages.

**Features:**
- Package installation and uninstallation
- Dependency management
- Package updates and versioning
- Package search and discovery
- Package registry integration
- Export/import package lists
- Cache management
- CLI interface

**Usage:**
```bash
# Install a package
diligaf-pm install networking

# Install specific version
diligaf-pm install crypto 1.2.0

# List installed packages
diligaf-pm list

# Search for packages
diligaf-pm search "web security"

# Update a package
diligaf-pm update crypto

# Update all packages
diligaf-pm update-all

# Show package information
diligaf-pm info networking

# Uninstall a package
diligaf-pm uninstall old-package

# Export package list
diligaf-pm export my-packages.json

# Import package list
diligaf-pm import my-packages.json

# Clean cache
diligaf-pm clean-cache
```

## 🔧 **Library Structure**

```
libs/
├── core/
│   └── diligaf-core.diligaf          # Core utilities
├── networking/
│   └── diligaf-net.diligaf           # Networking and protocols
├── crypto/
│   └── diligaf-crypto.diligaf        # Cryptography and encryption
├── exploit/
│   └── diligaf-exploit.diligaf       # Exploit development
├── recon/
│   └── diligaf-recon.diligaf         # Reconnaissance and scanning
├── forensics/
│   └── diligaf-forensics.diligaf     # Digital forensics
├── malware/
│   └── diligaf-malware.diligaf       # Malware analysis
├── osint/
│   └── diligaf-osint.diligaf         # OSINT and intelligence
├── packet/
│   └── diligaf-packet.diligaf        # Packet crafting and analysis
├── web/
│   └── diligaf-web.diligaf           # Web application security
├── mobile/
│   └── diligaf-mobile.diligaf        # Mobile security
├── cloud/
│   └── diligaf-cloud.diligaf         # Cloud security
└── ai/
    └── diligaf-ai.diligaf            # AI/ML security
```

## 🚀 **Quick Start**

### **1. Install DILIGAF**
```bash
git clone https://github.com/yourusername/diligaf.git
cd diligaf
chmod +x install.sh
./install.sh
```

### **2. Install Core Libraries**
```bash
diligaf-pm install core
diligaf-pm install networking
diligaf-pm install crypto
diligaf-pm install exploit
```

### **3. Create Your First DILIGAF Program**
```diligaf
# hello.diligaf
stackit core
stackit net
stackit crypto

# System information
info = get_system_info()
print("Running on:", info["platform"])

# Network scan
open_ports = scan_common_ports("127.0.0.1")
print("Open ports on localhost:", open_ports)

# Encrypt a message
key = generate_key("aes", 256)
message = "Hello from DILIGAF!"
encrypted = encrypt_aes(message, key)
print("Encrypted message:", encrypted)

# Generate exploit payload
payload = generate_python_payload("reverse_shell", "192.168.1.100", 4444)
print("Generated payload:", payload)
```

### **4. Run Your Program**
```bash
diligaf hello.diligaf
```

## 🔍 **Advanced Usage**

### **Custom Library Development**
```diligaf
# my-custom-lib.diligaf
class MyCustomLib
    def __init__(self)
        self.name = "My Custom Library"
        self.version = "1.0.0"
    
    def custom_function(self, param)
        return f"Custom function called with: {param}"

# Export functionality
custom_lib = MyCustomLib()

def my_function(param)
    return custom_lib.custom_function(param)
```

### **Library Integration**
```diligaf
# advanced-exploit.diligaf
stackit core
stackit net
stackit crypto
stackit exploit

def advanced_exploit(target, port)
    # Network reconnaissance
    open_ports = scan_common_ports(target)
    print(f"Open ports on {target}: {open_ports}")
    
    # Generate encrypted payload
    key = generate_key("aes", 256)
    payload = generate_python_payload("reverse_shell", target, port)
    encrypted_payload = encrypt_aes(payload, key)
    
    # Obfuscate payload
    obfuscated = obfuscate_payload(encrypted_payload, "base64")
    
    # Test SQL injection
    sql_payload = generate_union_payload(3, "users", "1=1")
    result = test_sql_injection(f"http://{target}", "id", sql_payload)
    
    if result and result["vulnerable"]
        print("SQL injection vulnerability found!")
        return True
    else
        print("No SQL injection vulnerability found")
        return False

# Execute exploit
target = "192.168.1.100"
port = 80
success = advanced_exploit(target, port)
print(f"Exploit successful: {success}")
```

## 📚 **Documentation**

- **Core Library**: [libs/core/README.md](libs/core/README.md)
- **Networking Library**: [libs/networking/README.md](libs/networking/README.md)
- **Cryptography Library**: [libs/crypto/README.md](libs/crypto/README.md)
- **Exploit Library**: [libs/exploit/README.md](libs/exploit/README.md)
- **Package Manager**: [diligaf-pm.md](diligaf-pm.md)

## 🤝 **Contributing**

1. Fork the repository
2. Create a feature branch
3. Add your library to the appropriate directory
4. Update the package registry
5. Submit a pull request

## 📄 **License**

DILIGAF and its ecosystem are released under the MIT License. See [LICENSE](LICENSE) for details.

## 🆘 **Support**

- **Documentation**: [docs.diligaf.dev](https://docs.diligaf.dev)
- **Issues**: [GitHub Issues](https://github.com/yourusername/diligaf/issues)
- **Discussions**: [GitHub Discussions](https://github.com/yourusername/diligaf/discussions)
- **Discord**: [DILIGAF Discord](https://discord.gg/diligaf)

---

**DILIGAF Ecosystem** - The ultimate toolkit for security professionals! 🚀
