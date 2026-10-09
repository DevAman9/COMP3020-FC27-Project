library(jsonlite)

# ==================================================
# COMP3020 SOCIAL WEB ANALYTICS
# EA SPORTS FC 27 PROJECT
# SECTION 2.1 - DATA COLLECTION
# ==================================================

# Load private YouTube API key.
source("api_key.R")


# ==================================================
# CREATE DATA FOLDERS
# ==================================================

dir.create("data_raw", showWarnings = FALSE)
dir.create("data_clean", showWarnings = FALSE)


# ==================================================
# SELECTED VIDEOS
# ==================================================
#
# Final sample:
# - 3 Review / First Impressions videos
# - 3 Ultimate Team videos
# - 3 Career Mode videos
# - one selected video per creator
# - English-language creator videos
# - complete first-72-hour observation window
# - sufficient early comment activity
# ==================================================

selected.videos = data.frame(

  category = c(
    "Review",
    "Review",
    "Review",
    "Ultimate Team",
    "Ultimate Team",
    "Ultimate Team",
    "Career Mode",
    "Career Mode",
    "Career Mode"
  ),

  videoId = c(
    "ArCjNDHyn80",
    "PSDe1xhBpfU",
    "AoBvkZJHDrM",
    "Mbzz0gwTONU",
    "-iCLBdPChD0",
    "HaRFo7WlrJg",
    "tUUR5X5SNc0",
    "AtMIZq5ukD8",
    "EnXDvcc3TqM"
  ),

  title = c(
    "EA Sports FC 27 Review - The Final Verdict",
    "My Initial Thoughts on The FC 27 Gameplay!",
    "FC 27 First Gameplay Impressions - I Expected More, EA",
    "30x DEBUT ICON PACKS! FC 27 Ultimate Team",
    "WOW This SUCK So Bad... FC27 Ultimate Team",
    "People are NOT happy with this! FC27 Ultimate Team",
    "I Fixed LIVERPOOLS Biggest PROBLEMS In FC 27 Career Mode!",
    "MAN UTD FC 27 CAREER MODE! EPISODE 1",
    "I Takeover Chelsea in FC27 Career Mode!"
  ),

  creator = c(
    "GamingBolt",
    "Inception FC",
    "Machete Gaming",
    "AuzioMF",
    "AA9skillz",
    "NepentheZ",
    "CaniSports",
    "Mark Goldbridge",
    "BFordLancer"
  ),

  publishedAt = c(
    "2026-09-22T09:45:17Z",
    "2026-09-19T16:57:21Z",
    "2026-09-18T15:58:42Z",
    "2026-09-18T00:37:44Z",
    "2026-09-21T14:00:00Z",
    "2026-09-22T18:02:53Z",
    "2026-09-19T17:25:51Z",
    "2026-09-18T17:22:53Z",
    "2026-09-20T13:00:28Z"
  ),

  stringsAsFactors = FALSE
)

write.csv(
  selected.videos,
  "data_raw/selected_videos.csv",
  row.names = FALSE
)


# ==================================================
# HELPER FUNCTION: AUTHOR CHANNEL ID
# ==================================================

get.author.id = function(author.field, number.rows) {

  if (is.null(author.field)) {
    return(rep(NA_character_, number.rows))
  }

  if (
    is.data.frame(author.field) &&
    "value" %in% names(author.field)
  ) {
    return(author.field$value)
  }

  return(rep(NA_character_, number.rows))
}


# ==================================================
# COLLECT TOP-LEVEL COMMENTS FOR ONE VIDEO
# ==================================================

get.top.comments = function(video.id) {

  all.comments = data.frame()
  next.token = ""

  repeat {

    url = paste0(
      "https://www.googleapis.com/youtube/v3/commentThreads?",
      "part=snippet",
      "&videoId=", video.id,
      "&maxResults=100",
      "&order=time",
      "&textFormat=plainText",
      "&key=", youtube.api.key
    )

    if (next.token != "") {
      url = paste0(
        url,
        "&pageToken=",
        URLencode(next.token, reserved = TRUE)
      )
    }

    x = try(
      fromJSON(url),
      silent = TRUE
    )

    if (inherits(x, "try-error")) {
      stop(
        paste(
          "Could not retrieve top-level comments for video",
          video.id
        )
      )
    }

    if (
      is.null(x$items) ||
      length(x$items) == 0
    ) {
      break
    }

    thread = x$items$snippet
    top = thread$topLevelComment
    comment = top$snippet

    number.comments = nrow(comment)

    author.id = get.author.id(
      comment$authorChannelId,
      number.comments
    )

    temp = data.frame(
      videoId = rep(video.id, number.comments),
      commentId = top$id,
      author = comment$authorDisplayName,
      authorChannelId = author.id,
      commentText = comment$textDisplay,
      likeCount = comment$likeCount,
      publishedAt = comment$publishedAt,
      replyCount = thread$totalReplyCount,
      stringsAsFactors = FALSE
    )

    all.comments = rbind(
      all.comments,
      temp
    )

    if (
      is.null(x$nextPageToken) ||
      length(x$nextPageToken) == 0
    ) {
      break
    }

    next.token = x$nextPageToken
  }

  # YouTube pagination can occasionally return the same
  # top-level comment on two pages. Keep each comment once.
  if (nrow(all.comments) > 0) {
    all.comments = all.comments[
      !duplicated(all.comments$commentId),
    ]

    rownames(all.comments) = NULL
  }

  return(all.comments)
}


# ==================================================
# COLLECT ALL REPLIES TO ONE TOP-LEVEL COMMENT
# ==================================================

get.replies = function(
  parent.comment.id,
  video.id,
  parent.author,
  parent.author.id
) {

  all.replies = data.frame()
  next.token = ""

  repeat {

    url = paste0(
      "https://www.googleapis.com/youtube/v3/comments?",
      "part=snippet",
      "&parentId=",
      URLencode(parent.comment.id, reserved = TRUE),
      "&maxResults=100",
      "&textFormat=plainText",
      "&key=", youtube.api.key
    )

    if (next.token != "") {
      url = paste0(
        url,
        "&pageToken=",
        URLencode(next.token, reserved = TRUE)
      )
    }

    x = try(
      fromJSON(url),
      silent = TRUE
    )

    if (inherits(x, "try-error")) {
      cat(
        "Could not retrieve replies for comment:",
        parent.comment.id,
        "\n"
      )
      break
    }

    if (
      is.null(x$items) ||
      length(x$items) == 0
    ) {
      break
    }

    reply = x$items$snippet
    number.replies = nrow(reply)

    reply.author.id = get.author.id(
      reply$authorChannelId,
      number.replies
    )

    temp = data.frame(
      videoId = rep(video.id, number.replies),
      replyId = x$items$id,
      parentCommentId = reply$parentId,
      parentAuthor = rep(parent.author, number.replies),
      parentAuthorChannelId = rep(parent.author.id, number.replies),
      replyAuthor = reply$authorDisplayName,
      replyAuthorChannelId = reply.author.id,
      replyText = reply$textDisplay,
      likeCount = reply$likeCount,
      publishedAt = reply$publishedAt,
      stringsAsFactors = FALSE
    )

    all.replies = rbind(
      all.replies,
      temp
    )

    if (
      is.null(x$nextPageToken) ||
      length(x$nextPageToken) == 0
    ) {
      break
    }

    next.token = x$nextPageToken
  }

  # Safety check in case the same reply appears on
  # more than one API page.
  if (nrow(all.replies) > 0) {
    all.replies = all.replies[
      !duplicated(all.replies$replyId),
    ]

    rownames(all.replies) = NULL
  }

  return(all.replies)
}


# ==================================================
# COLLECT DATA FOR ALL NINE VIDEOS
# ==================================================

all.top.comments = data.frame()
all.replies = data.frame()

for (i in 1:nrow(selected.videos)) {

  video.id = selected.videos$videoId[i]
  video.category = selected.videos$category[i]
  video.title = selected.videos$title[i]
  video.creator = selected.videos$creator[i]

  cat("\n========================================\n")
  cat("Collecting video", i, "of", nrow(selected.videos), "\n")
  cat(video.creator, "-", video.title, "\n")
  cat("========================================\n")

  video.time = as.POSIXct(
    selected.videos$publishedAt[i],
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  cutoff.time = video.time + 72 * 60 * 60

  comments = get.top.comments(video.id)

  comments$publishedTime = as.POSIXct(
    comments$publishedAt,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  comments = comments[
    !is.na(comments$publishedTime) &
      comments$publishedTime >= video.time &
      comments$publishedTime <= cutoff.time,
  ]

  comments$category = video.category
  comments$videoTitle = video.title
  comments$creator = video.creator

  all.top.comments = rbind(
    all.top.comments,
    comments
  )

  cat(
    "Unique top-level comments in first 72 hours:",
    nrow(comments),
    "\n"
  )

  # --------------------------------------------------
  # ACTUAL REPLIES
  # --------------------------------------------------

  comments.with.replies = comments[
    comments$replyCount > 0,
  ]

  video.replies = data.frame()

  if (nrow(comments.with.replies) > 0) {

    for (j in 1:nrow(comments.with.replies)) {

      replies = get.replies(
        comments.with.replies$commentId[j],
        video.id,
        comments.with.replies$author[j],
        comments.with.replies$authorChannelId[j]
      )

      if (nrow(replies) > 0) {

        replies$publishedTime = as.POSIXct(
          replies$publishedAt,
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )

        replies = replies[
          !is.na(replies$publishedTime) &
            replies$publishedTime >= video.time &
            replies$publishedTime <= cutoff.time,
        ]

        if (nrow(replies) > 0) {

          replies$category = video.category
          replies$videoTitle = video.title
          replies$creator = video.creator

          video.replies = rbind(
            video.replies,
            replies
          )
        }
      }
    }
  }

  if (nrow(video.replies) > 0) {

    # Safety check after combining all reply threads
    # for the current video.
    video.replies = video.replies[
      !duplicated(video.replies$replyId),
    ]

    rownames(video.replies) = NULL

    all.replies = rbind(
      all.replies,
      video.replies
    )
  }

  cat(
    "Unique actual replies in first 72 hours:",
    nrow(video.replies),
    "\n"
  )
}


# ==================================================
# FINAL DUPLICATE SAFETY CHECKS
# ==================================================

all.top.comments = all.top.comments[
  !duplicated(all.top.comments$commentId),
]

rownames(all.top.comments) = NULL

if (nrow(all.replies) > 0) {
  all.replies = all.replies[
    !duplicated(all.replies$replyId),
  ]

  rownames(all.replies) = NULL
}


# ==================================================
# SAVE COMPLETE 72-HOUR DATA
# ==================================================

write.csv(
  all.top.comments,
  "data_raw/fc27_top_level_comments_72h.csv",
  row.names = FALSE
)

write.csv(
  all.replies,
  "data_raw/fc27_replies_72h.csv",
  row.names = FALSE
)


# ==================================================
# CREATE BALANCED ANALYSIS SAMPLE
# ==================================================
#
# Sections 2.2, 2.3 and 2.4 use exactly 80 comments
# from each of the nine videos:
#
# 80 x 9 = 720 comments.
#
# This prevents videos with much larger comment sections
# from dominating the text analyses.
# ==================================================

set.seed(3020)

analysis.comments = data.frame()

for (i in 1:nrow(selected.videos)) {

  video.id = selected.videos$videoId[i]

  video.comments = all.top.comments[
    all.top.comments$videoId == video.id,
  ]

  if (nrow(video.comments) < 80) {
    stop(
      paste(
        "Video",
        video.id,
        "has fewer than 80 unique comments."
      )
    )
  }

  chosen.rows = sample(
    1:nrow(video.comments),
    80,
    replace = FALSE
  )

  selected.comments = video.comments[
    chosen.rows,
  ]

  analysis.comments = rbind(
    analysis.comments,
    selected.comments
  )
}

analysis.comments$documentId = 1:nrow(analysis.comments)

write.csv(
  analysis.comments,
  "data_clean/fc27_comments_720.csv",
  row.names = FALSE
)


# ==================================================
# COLLECTION SUMMARY
# ==================================================

collection.summary = data.frame()

for (i in 1:nrow(selected.videos)) {

  video.id = selected.videos$videoId[i]

  number.comments = sum(
    all.top.comments$videoId == video.id
  )

  if (nrow(all.replies) > 0) {
    number.replies = sum(
      all.replies$videoId == video.id
    )
  } else {
    number.replies = 0
  }

  result = data.frame(
    category = selected.videos$category[i],
    videoId = video.id,
    creator = selected.videos$creator[i],
    title = selected.videos$title[i],
    topLevelComments72h = number.comments,
    sampledComments = 80,
    replies72h = number.replies,
    stringsAsFactors = FALSE
  )

  collection.summary = rbind(
    collection.summary,
    result
  )
}

write.csv(
  collection.summary,
  "data_clean/collection_summary.csv",
  row.names = FALSE
)


# ==================================================
# FINAL CHECKS
# ==================================================

cat("\n\n========================================\n")
cat("DATA COLLECTION COMPLETE\n")
cat("========================================\n")

cat(
  "\nTotal unique top-level comments collected:",
  nrow(all.top.comments),
  "\n"
)

cat(
  "Total unique actual replies collected:",
  nrow(all.replies),
  "\n"
)

cat(
  "Balanced analysis comments:",
  nrow(analysis.comments),
  "\n"
)

cat("\nDuplicate top-level comment IDs remaining:\n")
print(sum(duplicated(all.top.comments$commentId)))

cat("\nDuplicate reply IDs remaining:\n")
print(sum(duplicated(all.replies$replyId)))

cat("\nComments by category:\n")
print(table(analysis.comments$category))

cat("\nComments by video:\n")
print(table(analysis.comments$videoId))

cat("\nCollection summary:\n")
print(collection.summary)

cat("\nFiles created:\n")
cat("data_raw/selected_videos.csv\n")
cat("data_raw/fc27_top_level_comments_72h.csv\n")
cat("data_raw/fc27_replies_72h.csv\n")
cat("data_clean/fc27_comments_720.csv\n")
cat("data_clean/collection_summary.csv\n")
