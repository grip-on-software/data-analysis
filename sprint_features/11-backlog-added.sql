SELECT ${f(join_cols, "issue", mask=1)},
    ${s(issue_key)} AS key,
    MAX(${f(join_cols, "sprint", mask=2, alias=T, sprint="interval_sprint")}) AS ${f(join_cols, "", mask=2, alias=F)},
    MAX(${s(story_points)}) AS story_points,
    MAX(${s(fix_version)}) AS fixversion
FROM gros.${t("issue")}
LEFT JOIN gros.${t("issue")} AS older_issue
ON ${j(issue_next_changelog, "issue", "older_issue")}
JOIN gros.${t("sprint")} AS interval_sprint
ON ${j(join_cols, "issue", "interval_sprint", 1)}
AND interval_sprint.sprint_id IN (${filter_sprint_ids})
AND ${t("issue")}.updated > ${s(sprint_open, sprint="interval_sprint")}
${s(issue_join)}
WHERE ${s(issue_not_done)}
AND ${s(issue_backlog)}
${s(project_condition, project="issue")}
${s(filter_condition)}
AND (${t("older_issue")}.changelog_id IS NULL ${s(filter_inverse, issue="older_issue", cond_op="OR")})
${g(join_cols, "issue", f("issue_key"), mask=1)}
