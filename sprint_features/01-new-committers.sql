-- Number of new developers (that have not worked on the project before)
SELECT ${f(join_cols, "new_developer")}, COUNT(*) AS num_new_developers FROM (
    SELECT DISTINCT
        ${f(join_cols, "commits1")},
        CASE WHEN vcs_developer.jira_dev_id IS NOT NULL AND vcs_developer.jira_dev_id > 0
            THEN vcs_developer.jira_dev_id
            ELSE -commits1.developer_id
        END AS developer_id
    FROM
        (SELECT DISTINCT ${f(join_cols, "commits")}, commits.developer_id
        FROM gros.commits) AS commits1
        LEFT JOIN gros.vcs_developer
		ON commits1.developer_id = vcs_developer.alias_id

    LEFT JOIN (
        (SELECT DISTINCT ${f(join_cols, "commits")}, commits.developer_id
        FROM gros.commits)) AS commits2
        LEFT JOIN gros.vcs_developer AS developer2
		ON commits2.developer_id = developer2.alias_id

    ON ${j(join_cols[1], "commits2", "commits1")}
    AND (commits1.developer_id = -commits2.developer_id OR vcs_developer.alias_id = developer2.alias_id)
    AND commits2.sprint_id < commits1.sprint_id
    AND commits2.sprint_id <> 0
    WHERE commits1.sprint_id <> 0 AND commits2.sprint_id IS NULL
    AND vcs_developer.jira_dev_id <> -1
) AS new_developer
GROUP BY ${f(join_cols, "new_developer")}
