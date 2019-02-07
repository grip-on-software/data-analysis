SELECT
  ${t("developer")}.display_name AS source,
  ${s(project_name)} AS target,
  ${s(developer_internal)} AS internal,
  NOT (${s(project_core)}) AS support,
  COALESCE(SUM(commits_count.commits), 0) AS num_commits,
  COALESCE(SUM(issue_update.issues), 0) AS num_issues,
  ${t("developer")}.encryption AS encryption
FROM gros.project_developer
  JOIN gros.${t("developer")} ON ${j(join_cols, "project_developer", "developer", mask=2, var=T)}
  JOIN gros.${t("project")} ON ${j(join_cols, "project_developer", "project", mask=1)}
  LEFT JOIN (
         SELECT ${f(join_cols, "issue", mask=1)}, assignee,
			COUNT(DISTINCT issue_id) as issues
         FROM gros.${t("issue")} ${s(interval_condition, field='updated')}
		 GROUP BY ${f(join_cols, "issue", mask=1)}, assignee
       ) AS issue_update
	ON ${j(join_cols, "project_developer", "issue_update", mask=1)}
    AND ${s(developer_issue_name)} = issue_update.assignee
  LEFT JOIN gros.vcs_developer ON ${j(join_cols, "developer", "vcs_developer", mask=2, var=T)}
  LEFT JOIN (
         SELECT ${f(join_cols, "commits", mask=1)}, developer_id, COUNT(DISTINCT version_id) as commits
         FROM gros.commits ${s(interval_condition, field='commit_date')}
		 GROUP BY ${f(join_cols, "commits", mask=1)}, developer_id
       ) AS commits_count
    ON ${j(join_cols, "project_developer", "commits_count", mask=1)}
    AND vcs_developer.alias_id = commits_count.developer_id
  GROUP BY ${t("developer")}.display_name, ${f(join_cols, "project", mask=1)}, ${t("project")}.name, ${f("project_core")}, ${f("developer_internal")}, ${t("developer")}.encryption
  HAVING SUM(issue_update.issues) IS NOT NULL
  ORDER BY ${t("developer")}.display_name, ${t("project")}.name, ${f(join_cols, "project", mask=1)};
