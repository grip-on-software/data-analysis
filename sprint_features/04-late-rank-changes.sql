SELECT ${f(join_cols, "issue")}, COUNT(*) AS num_late_rank_changes
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
${s(issue_join)}
WHERE ${s(sprint_id, sprint="issue")} <> 0 AND ${t("issue")}.rank_change IS NOT NULL
AND ${t("issue")}.updated > ${s(planned_end)}
${s(project_condition, project="issue")}
${g(join_cols, "issue")}
