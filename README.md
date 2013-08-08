Some scripts I've used to help with tag and ignore information in a git-svn migration

Tags in SVN are basically full branches, so you can see the history of past
versions, and also the difference between basing a new tag off of trunk or off
of another tag - it's also possible to commit changes directly to a tag!

To keep this history, git-svn translates tags to full branches and commit
objects, named under refs/remotes/tags/. To keep this history without
excess branches, I decided to save those refs out of the way under
refs/tag-history/, but also to try to make corresponding git tags for the
benefit of tools like git-describe that treat tags specially.

migrate-tag.sh REF [parent]
takes a REF naming the commit translated from the head of an SVN tag,
probably refs/remotes/tag/$TAG, and tries to make corresponding git tags.

If there isn't a unique medrge-base, it exists with an error.

If REF has a unique merge-base against the parent branch, defaulting to master,
it makes an annotated tag named $(dirname REF) with the same committer and
message as the commit named by REF (this tag object doesn't record
whether the tag had ever been moved or if the current value was actually
tagged against trunk or against anopther tag, but I don't think that's
too important).

If the merge-base has a different tree, I make a lightweight tag
to REF itself so you can get the contents, and a lightweight tag
named lca/$(dirname REF) to the merge-base, so tools will show you
where on the parent branch the tag begin.

conv-tag.sh COMMIT TAG TARGET
is a helper script for migrate-tag.sh, generating git-mktag input
for a tag named TAG to an object named by TARGET by 
rewriting the headers in git cat-file output for the commit named COMMIT.

CheckIgnores.hs
Mostly for interactive use.
Some definitions of the semantics of .gitignore files,
a parser for svn propgetn -R svn:ignore output, and some generic
tree stuff for checking whether a proposed set of ignores implies
all the ignores read from SVN.
Bug: uses git interpretation of ignore strings, even for the ones
parsed directly from SVN information
Uses recursion-schemes, free, and maybe some other hackage packages

Here.hs
a simple here-document quasiquoter, for putting svn propget output
directly in CheckIgnores.hs, to avoid doing IO after reloading
the file in ghci
