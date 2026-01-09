# climate-exposure

## Getting Started
### Create Your Own Copy (Fork & Clone)
1. **Fork** this repository on GitHub by clicking the "Fork" button at the top right.
2. **Clone** your personal fork (replace `YOUR-USERNAME` with your GitHub name):
```
git clone git@github.com:YOUR-USERNAME/climate-exposure.git
cd climate-exposure
```

Add the Upstream Remote: This allows you to sync your fork with the main team repo:
```
git remote add upstream git@github.com:daveschaaf/climate-exposure.git
```

Confirm the remotes:
```
git remote -v
```
* `origin` pointing to **your** GitHub username
* `upstream` pointing to **daveschaaf**


## Development Workflow
### Create a Feature Branch
Before starting any work, ensure your local main is up to date and create a new branch:
```
git checkout main
git pull upstream main
```

Create a new branch for your specific task
```
git checkout -b your-feature-name
```

### Commit Your Changes
```
git add .
git commit -m "feat: some cool new feature"
```

### Push and Submit a Pull Request
Push your branch to your GitHub fork:
```
git push origin your-feature-name
```

### Open a Pull Request (PR):
Navigate to the original daveschaaf/climate-exposure repository on GitHub.
You should see a yellow bar saying "Compare & pull request." Click it.
Describe your changes, link any relevant issues, and tag a teammate for review.

