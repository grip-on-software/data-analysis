-- Number of VCS committers in sprint
SELECT ${f(join_cols, "sprint_devs")}, COUNT(*) AS number_of_vcs_devs FROM (
    SELECT ${f(join_cols, "commits")}, commits.developer_id FROM gros.commits
    JOIN gros.vcs_developer ON commits.developer_id = vcs_developer.alias_id
    WHERE vcs_developer.jira_dev_id > 0
    GROUP BY ${f(join_cols, "commits")}, commits.developer_id
    HAVING ${s(sprint_id, sprint="commits")} <> 0
) AS sprint_devs
${g(join_cols, "sprint_devs")}
