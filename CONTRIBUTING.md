# 🤝 Contributing to Good Vibes Scaner

*Spreading good vibes through collaborative security! ✨*

First off, thanks for taking the time to contribute! You're helping make the internet more secure with good vibes! 🚀

## 🌟 Code of Conduct

This project is committed to maintaining good vibes for everyone:

- 🤗 Be welcoming and inclusive
- 💫 Be respectful and constructive
- 🎯 Focus on what's best for the community  
- 🧘 Keep those vibes positive and supportive
- 🚀 Help others learn and grow

## 🔍 How Can I Contribute?

### 🐛 Reporting Bugs (When Vibes Go Wrong)

Before creating bug reports, please check the existing issues to avoid duplicates. When you create a bug report, include:

- 📝 A clear and descriptive title
- 🔬 Detailed steps to reproduce the issue
- 💻 Your operating system and GHC version
- 📋 Expected vs actual behavior
- 🔍 Any relevant code samples or repository examples
- 🌟 Keep those bug reports friendly and constructive!

### 💡 Suggesting Enhancements (Spreading New Vibes)

Enhancement suggestions are welcome! Please include:

- 📝 A clear and descriptive title
- 🎯 A detailed description of the proposed enhancement
- 💫 Why this enhancement would bring good vibes to users
- 🔄 Any alternative approaches you've considered
- 📊 Examples of how this would work

### 🛠️ Code Contributions (Coding With Good Vibes)

#### Development Setup

```bash
# Clone the good vibes
git clone https://github.com/yourusername/good-vibes-scaner.git
cd good-vibes-scaner

# Install dependencies
cabal update
cabal install --dependencies-only --enable-tests

# Build and test
cabal build
cabal test
```

#### Coding Standards (Keeping Code Vibes High)

- 📐 Follow standard Haskell conventions
- 🎨 Use meaningful variable names (spread those good naming vibes!)
- 📝 Add Haddock documentation for all exported functions
- 🧪 Write tests for new functionality
- 🚀 Keep functions pure when possible
- 💫 Use descriptive commit messages in the format: `type: what this commit does`
- ✨ Follow conventional commit format (e.g., `feat:`, `fix:`, `docs:`, `test:`, `refactor:`)
- 🎯 Write commit messages as if completing the sentence "This commit will..."
- ✨ Follow the existing code style and patterns

#### Testing Your Vibes

```bash
# Run the full test suite
cabal test

# Run HLint for code quality vibes
hlint app/ src/ test/

# Test your changes on real repositories
cabal run good-vibes-scaner -- --current-only /path/to/test/repo
```

#### Module Organization (Organizing Those Good Vibes)

- `GoodVibes.Core` - Main scanning logic and coordination
- `GoodVibes.Types` - Data types and configurations
- `GoodVibes.Patterns` - Secret pattern matching and classification
- `GoodVibes.Git` - Git repository operations
- `GoodVibes.Scanner` - File scanning and analysis
- `GoodVibes.Utils` - Helper functions and utilities

### 📚 Documentation (Documenting Good Vibes)

Documentation improvements are always welcome:

- 📝 README updates
- 🔍 Code documentation and examples
- 📋 Tutorial content
- 🎯 Usage examples
- 💡 Best practices guides

## 🔄 Pull Request Process (Merging Good Vibes)

1. 🍴 Fork the repository
2. 🌿 Create a feature branch from `main`
   ```bash
   git checkout -b feature/amazing-new-vibes
   ```

3. ✨ Make your changes
   - Keep commits focused and atomic
   - Write clear commit messages
   - Follow the coding standards

4. 🧪 Add or update tests
   - Ensure new functionality is tested
   - Verify existing tests still pass
   - Add property-based tests where appropriate

5. 📝 Update documentation
   - Update README if needed
   - Add/update function documentation
   - Update CHANGELOG if applicable

6. ✅ Verify your changes
   ```bash
   cabal build
   cabal test
   hlint app/ src/ test/
   ```

7. 🚀 Submit the pull request
   - Use a clear and descriptive title
   - Explain what changes you made and why
   - Reference any related issues
   - Include screenshots or examples if applicable

8. 🤝 Respond to feedback
   - Be open to suggestions and constructive criticism
   - Make requested changes promptly
   - Keep the conversation positive and collaborative

## 🏷️ Issue Labels (Organizing Those Issue Vibes)

We use labels to organize issues and PRs:

- 🐛 `bug` - Something isn't working with the good vibes
- ✨ `enhancement` - New features that would bring good vibes
- 📚 `documentation` - Documentation improvements
- 🆘 `help wanted` - Community input needed
- 🎯 `good first issue` - Perfect for newcomers to spread their first vibes
- 🔥 `priority: high` - Urgent vibes needed
- 🚀 `performance` - Speed and efficiency improvements
- 🧪 `testing` - Test coverage and quality improvements

## 🚀 Release Process (Releasing Good Vibes)

For maintainers releasing new versions:

1. 📝 Update version in `good-vibes-scaner.cabal`
2. 📋 Update `CHANGELOG.md` with new features and fixes
3. 🏷️ Create and push a new tag: `git tag v1.0.1 && git push origin v1.0.1`
4. 🤖 GitHub Actions will automatically build and create the release
5. 🎉 Celebrate spreading more good vibes to the world!

## 💬 Getting Help (When You Need Some Vibes)

- 💭 Start a [Discussion](https://github.com/yourusername/good-vibes-scaner/discussions) for questions
- 🐛 Create an [Issue](https://github.com/yourusername/good-vibes-scaner/issues) for bugs
- 💡 Join our community chat (link coming soon!)
- 📧 Email the maintainers for private concerns

## 🎯 Areas Where We Need Good Vibes

Current areas where contributions would bring especially good vibes:

- 🔍 **Pattern Detection**: New secret types and improved accuracy
- 🚀 **Performance**: Optimizations for large repositories
- 🧪 **Testing**: More comprehensive test coverage
- 📚 **Documentation**: Usage examples and best practices
- 🌐 **Internationalization**: Multi-language support
- 🔧 **Integrations**: IDE plugins and tool integrations

## 🙏 Recognition (Spreading Contributor Vibes)

All contributors get:

- 🌟 Listed in our contributors section
- 💫 Eternal gratitude and good vibes
- 🏆 Recognition in release notes
- 🚀 The satisfaction of making the internet more secure!

---

*Thank you for contributing to Good Vibes Scaner! Together, we're making security more accessible and spreading good vibes throughout the developer community!* ✨🚀

*Keep those contributions coming and those vibes positive!* 💫