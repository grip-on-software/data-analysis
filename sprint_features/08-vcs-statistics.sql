SELECT ${f(join_cols, "commits")},
    COUNT(*) AS num_commits,
    AVG(commits.insertions) AS avg_insertions,
    AVG(commits.deletions) AS avg_deletions,
    AVG(commits.size_of_commit) AS avg_size,
    AVG(commits.number_of_files) AS avg_files,
    AVG(commits.number_of_lines) AS avg_lines
FROM gros.commits
GROUP BY ${f(join_cols, "commits")} HAVING commits.sprint_id <> 0
