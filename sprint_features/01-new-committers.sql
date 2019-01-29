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

    ON ${j(join_cols, "commits2", "commits1", 1)}
    AND (commits1.developer_id = -commits2.developer_id OR vcs_developer.alias_id = developer2.alias_id)
    AND ${s(sprint_id, sprint="commits2")} < ${s(sprint_id, sprint="commits1")}
    AND ${s(sprint_id, sprint="commits2")} <> 0
    WHERE ${s(sprint_id, sprint="commits1")} <> 0
	AND ${f(join_cols, "commits2", mask=2, alias="alias")} IS NULL
    AND vcs_developer.jira_dev_id <> -1
	${s(project_condition, project="commits1")}
) AS new_developer
${g(join_cols, "new_developer")}
