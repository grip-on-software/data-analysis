SELECT project.name, sprint.start_date, COUNT(DISTINCT commits.developer_id) AS num_committers, num_not_done
FROM ((
   (gros.commits INNER JOIN gros.project ON commits.project_id = project.project_id)
    INNER JOIN gros.sprint ON commits.sprint_id = sprint.sprint_id)
    LEFT OUTER JOIN (SELECT issue.project_id, issue.sprint_id, COUNT(*) AS num_not_done FROM gros.issue,
        (SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata
        WHERE issue.issue_id = maxdata.issue_id
        AND issue.changelog_id = maxdata.max_changelog_id
        AND issue.resolution <> 1 AND issue.status <> 6
        GROUP BY issue.project_id, issue.sprint_id
    ) AS issuedata ON issuedata.project_id = project.project_id AND issuedata.sprint_id = sprint.sprint_id
)
GROUP BY project.name, sprint.start_date, num_not_done
ORDER BY project.name, sprint.start_date;
