SELECT project_id, COUNT(*) AS num_links
FROM gros.issuelink
JOIN gros.issue ON issuelink.from_key = issue.key AND issue.changelog_id = 0
GROUP BY project_id;
