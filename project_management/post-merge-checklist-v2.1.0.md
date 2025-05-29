# Post-Merge Checklist for v2.1.0

**Created**: 2025-05-30  
**Purpose**: Track actions after merging PR #11

## Immediate Actions (Within 5 minutes)
- [ ] Verify merge completed successfully
- [ ] Switch to main branch: `git checkout main`
- [ ] Pull latest changes: `git pull origin main`
- [ ] Verify version in main: `grep version src/ecc-variables.el`

## Release Actions (Within 30 minutes)
- [ ] Create annotated tag:
  ```bash
  git tag -a v2.1.0 -m "Release version 2.1.0
  
  Major features:
  - Yank-as-file for vterm buffers
  - Auto-periodical command execution  
  - Periodic return sending
  - Enhanced buffer management
  
  Bug fixes:
  - Modeline buffer awareness
  - CI/CD pipeline issues
  
  See CHANGELOG.md for full details."
  ```

- [ ] Push tag: `git push origin v2.1.0`

- [ ] Create GitHub release:
  ```bash
  gh release create v2.1.0 \
    --title "v2.1.0: Yank-as-file, Auto-periodical, and more" \
    --notes-file CHANGELOG.md \
    --target main
  ```

## Verification Actions
- [ ] Check release appears on GitHub
- [ ] Verify tag is visible: `git tag -l v2.1.0`
- [ ] Test installation from fresh clone
- [ ] Update any external documentation

## Communication Actions
- [ ] Post release announcement (prepared in release-announcement-v2.1.0.md)
- [ ] Consider posting to:
  - [ ] r/emacs subreddit
  - [ ] Emacs mailing lists
  - [ ] Personal blog/Twitter
  - [ ] Hacker News (if appropriate)

## Cleanup Actions
- [ ] Archive completed feature branches
- [ ] Close related issues
- [ ] Update project board/kanban
- [ ] Plan v2.2.0 features

## Monitoring (First 24 hours)
- [ ] Watch for user issues
- [ ] Monitor GitHub issues
- [ ] Be ready for hotfix if needed

## Success Metrics
- [ ] Users successfully installing
- [ ] No critical bugs reported
- [ ] Positive feedback received

---
This checklist ensures smooth release execution and follow-up.