SELECT issue_updaters.project_id, issue_updaters.sprint_id, AVG(issue_updaters.num_updaters) AS avg_updaters
FROM
    (SELECT updaters.project_id, issue.sprint_id, issue.issue_id, COUNT(*) AS num_updaters FROM
        (SELECT DISTINCT devs.project_id, devs.issue_id, devs.updater FROM
            (SELECT issue.project_id, issue.issue_id, issue.updated_by AS updater
            FROM gros.issue
            UNION ALL
            SELECT issue.project_id, issue.issue_id, comment.author AS updater
            FROM gros.comment JOIN gros.issue ON comment.issue_id = issue.issue_id
            ) AS devs
        ) AS updaters,
        (SELECT issue.issue_id, MAX(issue.changelog_id) AS changelog_id
        FROM gros.issue GROUP BY issue.issue_id) AS maxdata,
        gros.issue
    WHERE updaters.issue_id = maxdata.issue_id
    AND maxdata.issue_id = issue.issue_id AND maxdata.changelog_id = issue.changelog_id
    AND issue.sprint_id <> 0
    GROUP BY updaters.project_id, issue.sprint_id, issue.issue_id) AS issue_updaters
GROUP BY issue_updaters.project_id, issue_updaters.sprint_id
