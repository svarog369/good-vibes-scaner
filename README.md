# 🔍 Good Vibes Scaner ✨

*A vibed-out tool to catch secrets in your git repos with good vibes*

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green?style=for-the-badge)
![Build Status](https://img.shields.io/github/actions/workflow/status/svarog369/good-vibes-scaner/ci.yml?style=for-the-badge)

## 🌟 What's This Vibe About?

Good Vibes Scaner is a chill yet powerful security tool that vibes through your git repository history to find accidentally committed secrets like API keys, passwords, and other sensitive data. Built with Haskell and packed with good vibes! 🚀

### ✨ Features That Bring Good Vibes

- 🔍 **Smart Secret Detection** - Catches API keys, AWS credentials, private keys, JWT tokens, and more
- 🧠 **False Positive Filtering** - Smart enough to ignore CamelCase identifiers and common code patterns
- 📁 **Dual Scanning Mode** - Scan current files AND git history
- 🎯 **High Entropy Analysis** - Uses entropy calculation to detect real secrets
- 🧪 **Test-Aware** - More lenient with test files to reduce noise
- 🚀 **Fast & Efficient** - Built with Haskell for speed and reliability
- 💫 **Good Vibes Only** - Clean, friendly output that doesn't harsh your mellow

## 🚀 Quick Start (Keep Those Vibes Flowing)

### Installation

```bash
# Clone the good vibes
git clone https://github.com/svarog369/good-vibes-scaner.git
cd good-vibes-scaner

# Build with good vibes
cabal build

# Install for system-wide good vibes
cabal install
```

### Basic Usage

```bash
# Scan current directory + 50 commits (default good vibes)
good-vibes-scaner

# Just scan current files (quick good vibes)
good-vibes-scaner --current-only

# Scan more commit history (deep good vibes)
good-vibes-scaner --history-only 200

# Scan specific repo (spread the good vibes)
good-vibes-scaner /path/to/your/repo
```

## 🎨 What Secrets Get Caught in Our Vibe Check?

- 🔑 **API Keys** - `sk_`, `pk_`, `API_KEY` patterns
- ☁️ **AWS Access Keys** - `AKIA...` patterns  
- 🔐 **Private Keys** - `-----BEGIN PRIVATE KEY-----`
- 🗄️ **Database URLs** - Connection strings with credentials
- 🎫 **JWT Tokens** - `eyJ...` patterns
- 🌟 **High Entropy Secrets** - Random-looking strings that vibe like secrets

## 🛠️ Command Line Options

```
Usage: good-vibes-scaner [options] [path] [commits]

Arguments:
  path      Path to git repository (default: current directory)
  commits   Number of commits to scan (default: 50)

Options:
  --current-only    Only scan current working directory
  --history-only    Only scan git history  
  --no-current      Don't scan current working directory
  --no-history      Don't scan git history
  --help            Show this help message
```

## 🧪 Running Tests (Keep the Quality Vibes High)

```bash
# Run the test suite
cabal test

# Run tests with coverage
cabal test --enable-coverage

# Run specific test modules
cabal test --test-options="--match=\"Pattern matching\""
```

## 🏗️ Development Setup

### Prerequisites
- GHC 9.2.8+ (but 9.4.7 and 9.6.3 have the best vibes)
- Cabal 3.0+
- Git (for the good git vibes)

### Building from Source

```bash
# Clone and enter the vibe zone
git clone https://github.com/svarog369/good-vibes-scaner.git
cd good-vibes-scaner

# Install dependencies
cabal update
cabal install --dependencies-only

# Build with good vibes
cabal build

# Run tests to keep vibes high
cabal test
```

### Project Structure (Organized Vibes)

```
good-vibes-scaner/
├── app/                    # Executable entry point
│   └── Main.hs
├── src/                    # Library modules
│   └── GoodVibes/
│       ├── Core.hs         # Core scanning logic
│       ├── Types.hs        # Data types and config
│       ├── Patterns.hs     # Secret pattern matching
│       ├── Git.hs          # Git operations
│       ├── Scanner.hs      # File scanning logic
│       └── Utils.hs        # Utility functions
├── test/                   # Test suite
│   ├── Main.hs
│   └── GoodVibes/
│       ├── CoreSpec.hs
│       └── PatternsSpec.hs
├── good-vibes-scaner.cabal # Project configuration
└── README.md               # You are here! ✨
```

## 🤝 Contributing (Spread the Good Vibes)

We love contributions that bring good vibes! Here's how to vibe with us:

1. 🍴 Fork the repo
2. 🌿 Create a feature branch (`git checkout -b feature/amazing-vibes`)
3. ✨ Make your changes (keep those vibes positive)
4. 🧪 Add tests (test those vibes)
5. ✅ Run the test suite (`cabal test`)
6. 📝 Commit with good vibes (`git commit -m 'Add amazing vibes'`)
7. 🚀 Push your vibes (`git push origin feature/amazing-vibes`)
8. 🎯 Open a Pull Request

### Code Style Vibes

- Follow standard Haskell conventions
- Use meaningful variable names (good vibes in naming!)
- Add documentation for exported functions
- Keep functions pure when possible (pure vibes!)
- Write tests for new features (tested vibes are good vibes!)

## 📝 Examples (Good Vibes in Action)

### Scanning a React Project

```bash
# Quick scan before committing
good-vibes-scaner --current-only

# Deep dive into commit history  
good-vibes-scaner --history-only 100
```

### CI/CD Integration

```yaml
# Add to your GitHub Actions workflow
- name: Scan for secrets with good vibes
  run: |
    good-vibes-scaner --current-only
    if [ $? -ne 0 ]; then
      echo "❌ Secrets detected! Fix them to keep the good vibes flowing"
      exit 1
    fi
```

## 🔒 Security Notes (Keeping Those Vibes Secure)

- This tool helps detect accidentally committed secrets
- Always review findings manually (trust but verify the vibes)
- Consider using environment variables for secrets in production
- Rotate any exposed credentials immediately (bad vibes begone!)
- Add sensitive files to `.gitignore` to prevent future commits

## 📊 Performance (Optimized Good Vibes)

- **Current directory scanning**: ⚡ Super fast (< 1s for most projects)
- **History scanning**: 🐢 Depends on repo size (can be slow for large repos)
- **Memory usage**: 💾 Efficient (Haskell's lazy evaluation keeps memory vibes low)
- **CPU usage**: 🔥 Single-threaded but optimized

## 📜 License (Legal Good Vibes)

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments (Good Vibes Shoutouts)

- Built with ❤️ and Haskell
- Inspired by security best practices and good vibes
- Thanks to the Haskell community for the amazing ecosystem

## 🐛 Issues & Support (When Vibes Go Wrong)

Found a bug or have a feature request? We want to keep those vibes flowing smoothly!

- 🐛 [Report bugs](https://github.com/svarog369/good-vibes-scaner/issues/new?template=bug_report.md)
- 💡 [Request features](https://github.com/svarog369/good-vibes-scaner/issues/new?template=feature_request.md)
- 💬 [Start discussions](https://github.com/svarog369/good-vibes-scaner/discussions)

---

*Built with 💫 good vibes and Haskell magic* ✨

*Keep your repos secure and your vibes positive!* 🚀