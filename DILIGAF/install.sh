#!/bin/bash
# DILIGAF Installation Script
# Installs DILIGAF as a native command in the shell

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print banner
print_banner() {
    echo -e "${BLUE}"
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║  ██████╗ ██╗██╗     ██╗ ██████╗  █████╗ ███████╗  ║"
    echo "║  ██╔══██╗██║██║     ██║██╔════╝ ██╔══██╗██╔════╝  ║"
    echo "║  ██║  ██║██║██║     ██║██║  ███╗███████║█████╗    ║"
    echo "║  ██║  ██║██║██║     ██║██║   ██║██╔══██║██╔══╝    ║"
    echo "║  ██████╔╝██║███████╗██║╚██████╔╝██║  ██║██║       ║"
    echo "║  ╚═════╝ ╚╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝       ║"
    echo "║                                                      ║"
    echo "║  DILIGAF Installation System                          ║"
    echo "║  Embedding DILIGAF into your shell                    ║"
    echo "╚══════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

# Check if SBCL is installed
check_sbcl() {
    if ! command -v sbcl &> /dev/null; then
        echo -e "${RED}Error: SBCL (Steel Bank Common Lisp) is not installed.${NC}"
        echo "Please install SBCL first:"
        echo "  Ubuntu/Debian: sudo apt-get install sbcl"
        echo "  macOS: brew install sbcl"
        echo "  Windows: Download from https://www.sbcl.org/"
        exit 1
    fi
    echo -e "${GREEN}✓ SBCL found${NC}"
}

# Detect shell
detect_shell() {
    case "$SHELL" in
        */bash) echo "bash" ;;
        */zsh) echo "zsh" ;;
        */fish) echo "fish" ;;
        */sh) echo "sh" ;;
        *) echo "bash" ;;
    esac
}

# Get shell config file
get_shell_config() {
    local shell=$1
    case $shell in
        bash) echo "$HOME/.bashrc" ;;
        zsh) echo "$HOME/.zshrc" ;;
        fish) echo "$HOME/.config/fish/config.fish" ;;
        sh) echo "$HOME/.profile" ;;
        *) echo "$HOME/.bashrc" ;;
    esac
}

# Create DILIGAF directory
create_diligaf_dir() {
    local diligaf_dir="$HOME/.diligaf"
    mkdir -p "$diligaf_dir"
    echo "$diligaf_dir"
}

# Create DILIGAF wrapper script
create_wrapper() {
    local diligaf_dir=$1
    cat > "$diligaf_dir/diligaf" << 'EOF'
#!/bin/bash
# DILIGAF Wrapper Script
# This script embeds DILIGAF into the shell

DILIGAF_DIR="$HOME/.diligaf"
DILIGAF_EXECUTABLE="$DILIGAF_DIR/diligaf-native"

# Function to run DILIGAF
run_diligaf() {
    if [ -f "$DILIGAF_EXECUTABLE" ]; then
        "$DILIGAF_EXECUTABLE" "$@"
    else
        echo "DILIGAF not found. Please run 'diligaf-install' first."
        exit 1
    fi
}

# Function to show DILIGAF help
show_help() {
    echo "DILIGAF - Direct Interface for Low-level Intrusion and General-purpose Attack Framework"
    echo "Usage: diligaf [command] [options] [file]"
    echo ""
    echo "Commands:"
    echo "  --repl, -r          Start DILIGAF REPL"
    echo "  --compile, -c       Compile DILIGAF file to native"
    echo "  --run, -r           Run DILIGAF file"
    echo "  --help, -h          Show this help"
    echo "  --version, -v       Show version"
    echo "  --install           Install DILIGAF"
    echo "  --uninstall         Uninstall DILIGAF"
    echo ""
    echo "Examples:"
    echo "  diligaf --repl                    # Start REPL"
    echo "  diligaf --compile myfile.diligaf  # Compile to native"
    echo "  diligaf myfile.diligaf            # Run file"
    echo "  diligaf --version                 # Show version"
}

# Function to show version
show_version() {
    echo "DILIGAF v1.0.0 - Self-Hosting Hacker Language"
    echo "Built: $(date)"
    echo "Architecture: $(uname -m)"
    echo "OS: $(uname -s)"
}

# Function to install DILIGAF
install_diligaf() {
    echo "Installing DILIGAF..."
    mkdir -p "$DILIGAF_DIR"
    
    # Copy DILIGAF executable
    if [ -f "diligaf-native" ]; then
        cp diligaf-native "$DILIGAF_EXECUTABLE"
        chmod +x "$DILIGAF_EXECUTABLE"
        echo "✓ DILIGAF executable installed"
    else
        echo "✗ DILIGAF executable not found. Please build DILIGAF first."
        exit 1
    fi
    
    echo "DILIGAF installation complete!"
    echo "You can now use 'diligaf' command anywhere."
}

# Function to uninstall DILIGAF
uninstall_diligaf() {
    echo "Uninstalling DILIGAF..."
    rm -rf "$DILIGAF_DIR"
    echo "✓ DILIGAF uninstalled"
}

# Main command processing
case "$1" in
    --repl|-r)
        run_diligaf --repl
        ;;
    --compile|-c)
        if [ -z "$2" ]; then
            echo "Error: No file specified for compilation"
            exit 1
        fi
        run_diligaf --compile "$2" --output "${2%.*}"
        ;;
    --run|-r)
        if [ -z "$2" ]; then
            echo "Error: No file specified for execution"
            exit 1
        fi
        run_diligaf --script "$2"
        ;;
    --help|-h)
        show_help
        ;;
    --version|-v)
        show_version
        ;;
    --install)
        install_diligaf
        ;;
    --uninstall)
        uninstall_diligaf
        ;;
    --*)
        echo "Unknown option: $1"
        show_help
        exit 1
        ;;
    "")
        show_help
        ;;
    *)
        # If it's a file, run it
        if [ -f "$1" ]; then
            run_diligaf --script "$1"
        else
            echo "Error: File not found: $1"
            exit 1
        fi
        ;;
esac
EOF
    chmod +x "$diligaf_dir/diligaf"
}

# Create shell completion
create_completion() {
    local shell=$1
    local diligaf_dir=$2
    
    case $shell in
        bash)
            cat > "$diligaf_dir/diligaf-completion.bash" << 'EOF'
# DILIGAF Bash Completion
_diligaf_completion() {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local prev="${COMP_WORDS[COMP_CWORD-1]}"
    
    case "$prev" in
        --compile|-c)
            COMPREPLY=($(compgen -f -- "$cur" | grep '\.diligaf$'))
            ;;
        --run|-r)
            COMPREPLY=($(compgen -f -- "$cur" | grep '\.diligaf$'))
            ;;
        *)
            COMPREPLY=($(compgen -W '--repl --compile --run --help --version --install --uninstall' -- "$cur"))
            ;;
    esac
}

complete -F _diligaf_completion diligaf
EOF
            ;;
        zsh)
            cat > "$diligaf_dir/diligaf-completion.zsh" << 'EOF'
# DILIGAF Zsh Completion
_diligaf() {
    local context state line
    _arguments '1: :->command' '*::arg:->args'
    
    case $state in
        command)
            _values 'diligaf commands' \
                '--repl[Start DILIGAF REPL]' \
                '--compile[Compile DILIGAF file]' \
                '--run[Run DILIGAF file]' \
                '--help[Show help]' \
                '--version[Show version]' \
                '--install[Install DILIGAF]' \
                '--uninstall[Uninstall DILIGAF]'
            ;;
        args)
            case $line[1] in
                --compile|-c|--run|-r)
                    _files -g '*.diligaf'
                    ;;
            esac
            ;;
    esac
}

compdef _diligaf diligaf
EOF
            ;;
        fish)
            cat > "$diligaf_dir/diligaf-completion.fish" << 'EOF'
# DILIGAF Fish Completion
complete -c diligaf -s r -l repl -d 'Start DILIGAF REPL'
complete -c diligaf -s c -l compile -d 'Compile DILIGAF file' -r
complete -c diligaf -s r -l run -d 'Run DILIGAF file' -r
complete -c diligaf -s h -l help -d 'Show help'
complete -c diligaf -s v -l version -d 'Show version'
complete -c diligaf -l install -d 'Install DILIGAF'
complete -c diligaf -l uninstall -d 'Uninstall DILIGAF'
complete -c diligaf -f -a '*.diligaf'
EOF
            ;;
    esac
}

# Install to shell config
install_to_shell() {
    local shell=$1
    local diligaf_dir=$2
    local config_file=$(get_shell_config $shell)
    
    case $shell in
        bash|zsh)
            cat >> "$config_file" << EOF

# DILIGAF Configuration
export DILIGAF_HOME="$diligaf_dir"
export PATH="\$DILIGAF_HOME:\$PATH"

# DILIGAF completion
if [ -f "\$DILIGAF_HOME/diligaf-completion.$shell" ]; then
    source "\$DILIGAF_HOME/diligaf-completion.$shell"
fi

# DILIGAF alias
alias diligaf='\$DILIGAF_HOME/diligaf'
EOF
            ;;
        fish)
            cat >> "$config_file" << EOF

# DILIGAF Configuration
set -gx DILIGAF_HOME "$diligaf_dir"
set -gx PATH "\$DILIGAF_HOME" \$PATH

# DILIGAF completion
if test -f "\$DILIGAF_HOME/diligaf-completion.fish"
    source "\$DILIGAF_HOME/diligaf-completion.fish"
end

# DILIGAF alias
alias diligaf='\$DILIGAF_HOME/diligaf'
EOF
            ;;
    esac
}

# Build DILIGAF if needed
build_diligaf() {
    if [ ! -f "diligaf-native" ]; then
        echo -e "${YELLOW}Building DILIGAF...${NC}"
        if [ -f "build-diligaf.lisp" ]; then
            sbcl --script build-diligaf.lisp
        else
            echo -e "${RED}Error: build-diligaf.lisp not found${NC}"
            exit 1
        fi
    fi
    echo -e "${GREEN}✓ DILIGAF built${NC}"
}

# Copy DILIGAF files
copy_files() {
    local diligaf_dir=$1
    
    # Copy executable
    if [ -f "diligaf-native" ]; then
        cp diligaf-native "$diligaf_dir/"
        chmod +x "$diligaf_dir/diligaf-native"
        echo -e "${GREEN}✓ DILIGAF executable copied${NC}"
    else
        echo -e "${RED}Error: diligaf-native not found${NC}"
        exit 1
    fi
    
    # Copy source files
    local source_files=("diligaf-self-host.diligaf" "diligaf-compiler.diligaf" "diligaf-runtime.diligaf")
    for file in "${source_files[@]}"; do
        if [ -f "$file" ]; then
            cp "$file" "$diligaf_dir/"
            echo -e "${GREEN}✓ Copied $file${NC}"
        fi
    done
}

# Main installation
main() {
    print_banner
    
    echo -e "${BLUE}Installing DILIGAF to your shell...${NC}"
    echo "===================================="
    echo
    
    # Check prerequisites
    check_sbcl
    
    # Detect shell
    local shell=$(detect_shell)
    echo -e "${GREEN}✓ Detected shell: $shell${NC}"
    
    # Build DILIGAF if needed
    build_diligaf
    
    # Create DILIGAF directory
    echo "Creating DILIGAF directory..."
    local diligaf_dir=$(create_diligaf_dir)
    echo -e "${GREEN}✓ DILIGAF directory created: $diligaf_dir${NC}"
    
    # Create wrapper script
    echo "Creating DILIGAF wrapper script..."
    create_wrapper "$diligaf_dir"
    echo -e "${GREEN}✓ Wrapper script created${NC}"
    
    # Create completion
    echo "Creating shell completion..."
    create_completion "$shell" "$diligaf_dir"
    echo -e "${GREEN}✓ Shell completion created${NC}"
    
    # Copy files
    echo "Copying DILIGAF files..."
    copy_files "$diligaf_dir"
    
    # Install to shell
    echo "Installing DILIGAF to shell configuration..."
    install_to_shell "$shell" "$diligaf_dir"
    echo -e "${GREEN}✓ DILIGAF installed to shell${NC}"
    
    # Final instructions
    echo
    echo "===================================="
    echo -e "${GREEN}DILIGAF Installation Complete!${NC}"
    echo "===================================="
    echo
    echo "DILIGAF has been installed to: $diligaf_dir"
    echo "DILIGAF is now available as 'diligaf' command"
    echo
    echo "Usage:"
    echo "  diligaf --repl                    # Start REPL"
    echo "  diligaf --compile myfile.diligaf  # Compile to native"
    echo "  diligaf myfile.diligaf            # Run file"
    echo "  diligaf --version                 # Show version"
    echo
    echo "To start using DILIGAF:"
    echo "  1. Restart your shell or run: source $(get_shell_config $shell)"
    echo "  2. Type 'diligaf --repl' to start the REPL"
    echo "  3. Type 'diligaf --help' for more options"
    echo
    echo -e "${GREEN}DILIGAF is now embedded in your shell!${NC}"
    echo -e "${GREEN}Just type 'diligaf' and start hacking!${NC}"
    echo
}

# Run installation
main "$@"
