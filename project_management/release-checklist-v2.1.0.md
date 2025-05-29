# Release Checklist for v2.1.0

## Pre-Release Checks
- [x] All features implemented and tested
  - [x] Yank-as-file feature
  - [x] Auto-periodical commands
  - [x] Periodic return sending
- [x] Bug fixes completed
  - [x] Modeline buffer awareness
  - [x] CI/CD pipeline issues
- [x] Tests passing (91/91 locally, CI has known issues)
- [x] Version bumped to 2.1.0
- [x] CHANGELOG.md created and updated
- [x] Documentation updated

## Release Steps
1. [ ] **Merge PR #11**
   ```bash
   gh pr merge 11 --squash --subject "Release v2.1.0: Major features and improvements"
   ```

2. [ ] **Create Git Tag**
   ```bash
   git checkout main
   git pull origin main
   git tag -a v2.1.0 -m "Release version 2.1.0

   Major features:
   - Yank-as-file for vterm buffers
   - Auto-periodical command execution
   - Periodic return sending
   - Enhanced buffer management
   
   See CHANGELOG.md for full details."
   git push origin v2.1.0
   ```

3. [ ] **Create GitHub Release**
   ```bash
   gh release create v2.1.0 \
     --title "v2.1.0: Yank-as-file, Auto-periodical, and more" \
     --notes-file CHANGELOG.md \
     --target main
   ```

4. [ ] **Update README** (if needed)
   - Add examples for new features
   - Update installation instructions
   - Add configuration examples

5. [ ] **Post-Release Tasks**
   - [ ] Announce on relevant Emacs communities
   - [ ] Update project wiki/documentation
   - [ ] Create issues for known CI problems
   - [ ] Plan next release features

## Rollback Plan
If issues are discovered post-release:
1. `git revert` the merge commit
2. Fix issues in a hotfix branch
3. Release as v2.1.1

## Known Issues
- CI fails for Emacs 27.1, 28.2, 29.1 due to Ubuntu PPA compatibility
- These are marked as `continue-on-error` and don't block functionality

## Testing Checklist
- [x] Manual testing of all new features
- [x] Regression testing of existing features
- [x] Multi-buffer scenarios tested
- [x] Edge cases covered in tests

## Communication
- [ ] Update project README with release notes
- [ ] Consider posting to:
  - [ ] r/emacs
  - [ ] Emacs mailing lists
  - [ ] Personal blog/social media

---
Generated on: 2025-05-30