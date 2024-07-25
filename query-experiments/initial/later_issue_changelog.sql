SELECT issue.key, max_changelog_id, COUNT(issue.key) AS missing_ids
FROM gros.issue
JOIN (SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS max_issue
ON issue.issue_id = max_issue.issue_id
LEFT JOIN gros.issue AS next_issue
ON issue.issue_id = next_issue.issue_id AND issue.changelog_id = next_issue.changelog_id - 1
JOIN (SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id) AS max_issue
ON issue.issue_id = max_issue.issue_id AND issue.changelog_id < max_issue.changelog_id
WHERE next_issue.changelog_id IS NULL
GROUP BY issue.key, max_changelog_id;

SELECT issue.key, issue.changelog_id
FROM gros.issue
LEFT JOIN gros.issue AS next_issue
ON issue.issue_id = next_issue.issue_id AND issue.changelog_id = next_issue.changelog_id - 1
LEFT JOIN gros.issue AS later_issue
ON issue.issue_id = later_issue.issue_id AND issue.changelog_id = later_issue.changelog_id - 2
WHERE next_issue.changelog_id IS NULL
AND later_issue.changelog_id IS NOT NULL;

SELECT issue.key, issue.changelog_id, RANK() OVER (PARTITION BY issue.key ORDER BY issue.changelog_id ASC) AS new_changelog_id
FROM gros.issue
--WHERE issue.key = '<select issue key here>'
ORDER BY issue.key, issue.changelog_id;
