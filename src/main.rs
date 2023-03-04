use std::ops::Div;

use chrono::{DateTime, Duration, Utc};
use clap::{ColorChoice, Parser};
use color_eyre::eyre::{eyre, Result};
use nom::bytes::complete::{tag_no_case, take_until1, take_while1};
use plotters::{
    prelude::{BitMapBackend, ChartBuilder, IntoDrawingArea, LabelAreaPosition},
    series::LineSeries,
    style::{BLUE, WHITE},
};
use reqwest::{header::HeaderValue, Url};
use serde::{Deserialize, Serialize};
use simple_moving_average::{SumTreeSMA, SMA};

/// Possible LINK header labels.
/// We only care about the "next" and "last" ones.
/// This list may not be exhaustive.
/// See https://docs.github.com/en/rest/overview/resources-in-the-rest-api?apiVersion=2022-11-28#pagination
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Rel {
    Next,
    Previous,
    Last,
    First,
}

impl TryFrom<&str> for Rel {
    type Error = color_eyre::Report;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use Rel::*;

        match value {
            "next" => Ok(Next),
            "prev" => Ok(Previous),
            "last" => Ok(Last),
            "first" => Ok(First),
            _ => Err(eyre!("Invalid relation: {}", value)),
        }
    }
}

/// Representation of a single LINK header.
/// Useful for checking the format and
/// selecting the right entry by relation.
#[derive(Debug, PartialEq, Eq)]
struct GhLinkHeader {
    link: reqwest::Url,
    rel: Rel,
}

impl GhLinkHeader {
    pub fn from_header(value: &HeaderValue) -> Result<Vec<Self>> {
        value.to_str()?.split(", ").map(Self::try_from).collect()
    }
}

impl TryFrom<&str> for GhLinkHeader {
    type Error = color_eyre::Report;

    // GitHub returns its link headers in the following format:
    //
    // <url_string>; rel=\"prev\", <url_string>; rel=\"next\", <url_string>; rel=\"last\", <url_string>; rel=\"first\"
    //
    // This is fortunately fairly trivial to parse,
    // though disappointing that Reqwest doesn't include it.
    fn try_from(input: &str) -> std::result::Result<Self, Self::Error> {
        // Extract and discard the opening triangle bracket
        let (next_input, _) = tag_no_case::<_, _, nom::error::Error<_>>("<")(input)
            .map_err(|_| eyre!("Link header had no opening <"))?;

        // Extract everything up to the closing triangle bracket;
        // This gives the URL portion of the header
        let (next_input, link) = take_until1::<_, _, nom::error::Error<_>>(">")(next_input)
            .map_err(|_| eyre!("Link header had no closing >"))?;

        // Extract and discard the closing triangle bracket,
        // semicolon, whitespace and label which separate
        // the URL from the relation flag
        let (next_input, _) = tag_no_case::<_, _, nom::error::Error<_>>(">; rel=\"")(next_input)
            .map_err(|_| eyre!("Link header was not constructed with the expected separators"))?;

        // Extract the content of the "rel" section and
        // discard the rest of the input, as we've got what we need
        let (_, rel) =
            take_while1::<_, _, nom::error::Error<_>>(|c: char| c.is_ascii_alphabetic())(
                next_input,
            )
            .map_err(|_| eyre!("Link header rel portion had no alphanumeric content"))?;

        let rel = rel.try_into()?;

        Ok(GhLinkHeader {
            link: link.parse()?,
            rel,
        })
    }
}

struct WorkflowRunIterator {
    // Stash this so we don't have to go back to the args for it.
    // Sent in the Authorization header not as a query parameter.
    gh_token: String,
    // Storage for at most 100 runs from the API.
    // Technically this could be [Option<WorkflowRun>; 100]
    // and get filled with tombstones as we empty it,
    // but that's a bit more complicated than it needs to be
    // and the performance difference is negligible.
    cache: <Vec<WorkflowRun> as IntoIterator>::IntoIter,
    // Link to the next page of results, if available
    next_link: Option<Url>,
}

impl WorkflowRunIterator {
    /// Create a new iterator for the given repo and workflow file.
    /// We don't need to stash either of those in the WorkflowRunIterator
    /// because they're only used to construct the initial URL. The
    /// iterator will then use the LINK header to find the next page
    /// of results.
    fn try_new(gh_token: &str, repo: &str, workflow_file: &str) -> Result<Self> {
        Ok(WorkflowRunIterator {
            gh_token: gh_token.into(),
            cache: vec![].into_iter(),
            next_link: Some(format!(
                "https://api.github.com/repos/{repo}/actions/workflows/{workflow_file}/runs?status=success&per_page=100&page=1&created=>2022-06-01"
            ).parse()?),
        })
    }

    async fn try_next(&mut self) -> Result<Option<WorkflowRun>> {
        let next = self.cache.next();

        // If the cache isn't empty we can keep pulling off
        // that. Avoids needing to make hundreds of requests.
        if next.is_some() {
            return Ok(next);
        }

        // If the cache is empty and we have a "next" link
        // from the last request, we know there's another
        // page to fetch so we can refill it.
        if let Some(link) = self.next_link.as_ref() {
            // If we don't provide a user agent GitHub returns an error.
            // It doesn't seem to care if it's an entirely arbitrary
            // fake one, though.
            let client = reqwest::Client::builder()
                .user_agent("workflow_trends/0.1")
                .build()?;

            let response = client
                .get(link.clone())
                .bearer_auth(self.gh_token.as_str())
                .header("Accept", "application/vnd.github+json")
                // Unclear whether this version string matters,
                // but there doesn't seem to be a reason not to
                // send it.
                .header("X-GitHub-Api-Version", "2022-11-28")
                .send()
                .await?;

            // Parse all the LINK header entries, and then find the
            // one labelled "next". If there isn't one, we've run out.
            let next_link = response
                .headers()
                .get(reqwest::header::LINK)
                .map(GhLinkHeader::from_header)
                .and_then(|rel| {
                    rel.ok().and_then(|v| {
                        v.iter()
                            .find(|gh| gh.rel == Rel::Next)
                            .map(|gh| gh.link.clone())
                    })
                });

            let runs = response
                .json::<WorkflowRunResponse>()
                .await
                .unwrap()
                .workflow_runs;

            self.next_link = next_link;
            self.cache = runs.into_iter();
            return Ok(self.cache.next());
        }

        // The cache was empty and we didn't have a "next" link.
        // We're probably out, or at least GitHub doesn't feel
        // like giving us any more. Suspiciously, this reliably
        // happens at 1000 records.
        // TODO(Xymist): re-read GitHub docs to see if there's
        // another limit.
        Ok(None)
    }
}

#[derive(Deserialize, Debug)]
struct WorkflowRunResponse {
    workflow_runs: Vec<WorkflowRun>,
}

/// A single workflow run.
/// Contains the time at which the run started,
/// and the time at which it was last updated,
/// which is a good proxy for when it finished.
///
/// The API doesn't provide a "finished at" time,
/// nor a duration.
/// TODO(Xymist): check if this is still true at
/// some point.
///
/// Note: the API returns these as strings, so we
/// need to parse them into DateTime<Utc> ourselves.
/// Note2: We don't need everything the API returns,
/// so we leverage Serde's ability to recognise desired
/// fields and throw away the rest.
#[derive(Deserialize, Debug)]
struct WorkflowRun {
    run_started_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

impl WorkflowRun {
    pub fn duration(&self) -> Duration {
        self.updated_at - self.run_started_at
    }
}

/// A single data point in the set of workflow runs.
/// Contains the time at which the run started, and
/// the duration of the run in whole minutes. This
/// loses a little precision, but it's good enough.
#[derive(Debug, Clone, Serialize)]
struct DataPoint {
    started_at: DateTime<Utc>,
    duration_minutes: i64,
}

#[derive(Debug, Parser)]
#[command(
    version,
    about = "Fetches workflow runs and plots their duration trend",
    arg_required_else_help = true,
    color = ColorChoice::Always
)]
struct Args {
    /// A valid GitHub API personal access token,
    /// with sufficient permissions to call for
    /// workflow runs.
    /// (e.g. "ghp_1234567890abcdef1234567890abcdef123456")
    /// (see https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token)
    #[arg(short, long)]
    token: String,
    /// An owner/repository pair, as used in GitHub URLs
    /// (e.g. "Xymist/workflow_trends")
    #[arg(short, long)]
    repo: String,
    /// The name of the workflow file to fetch runs for
    #[arg(short, long, default_value = "tests.yml")]
    workflow_file: String,
    /// Discard any runs which are more than Nx or less
    /// than 1/N of the previous run. (e.g. 10x or 1/10th)
    /// Set to 0 to disable.
    #[arg(short, long, default_value = "3")]
    outlier_multiplier: i64,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let workflow_name = args
        .workflow_file
        .as_str()
        .trim_end_matches(".yml")
        .trim_end_matches(".yaml")
        .replace(' ', "_")
        .to_lowercase();

    let plot_name = format!("{}_log.png", workflow_name);
    let csv_name = format!("{}_log.csv", workflow_name);

    // Set up drawing canvas for timeline plot
    let root_area = BitMapBackend::new(plot_name.as_str(), (1920, 1080)).into_drawing_area();
    root_area.fill(&WHITE).unwrap();

    // Establish file and writer for CSV generation
    let csv_output = tokio::fs::File::create(csv_name.as_str()).await?;
    let mut wtr = csv_async::AsyncSerializer::from_writer(csv_output);

    // Establish accumulator to be later turned into timeline plot
    let mut acc: Vec<DataPoint> = Vec::new();

    let mut wri = WorkflowRunIterator::try_new(
        args.token.as_str(),
        args.repo.as_str(),
        args.workflow_file.as_str(),
    )?;

    let mut previous_duration = 0;

    while let Some(wr) = wri.try_next().await? {
        let point = DataPoint {
            started_at: wr.run_started_at,
            // The scale here goes to over an hour; seconds
            // are the default but realistically minutes are
            // what makes sense.
            duration_minutes: wr.duration().num_minutes(),
        };

        if check_outlier(
            previous_duration,
            point.duration_minutes,
            args.outlier_multiplier,
        ) {
            previous_duration = point.duration_minutes;
            wtr.serialize(point.clone()).await?;
            acc.push(point);
        }
    }

    wtr.flush().await?;

    // To avoid the charts looking like they go backwards,
    // ensure the time series datapoints are sorted.
    acc.sort_by(|a, b| a.started_at.cmp(&b.started_at));

    let start_date = acc
        .first()
        .map(|dp| dp.started_at)
        .ok_or(eyre!("Start date missing"))?;
    let end_date = acc
        .last()
        .map(|dp| dp.started_at)
        .ok_or(eyre!("End date missing"))?;
    let max_val = acc
        .iter()
        .map(|dp| dp.duration_minutes)
        .max()
        .ok_or(eyre!("Duration missing"))?;

    let mut ctx = ChartBuilder::on(&root_area)
        .set_label_area_size(LabelAreaPosition::Left, 40)
        .set_label_area_size(LabelAreaPosition::Bottom, 40)
        .caption("Workflow Runtime", ("sans-serif", 40))
        .build_cartesian_2d(start_date..end_date, 0..(max_val + max_val.div(10)))
        .unwrap();

    ctx.configure_mesh()
        .x_desc("Timestamp")
        .y_desc("Runtime (Minutes)")
        .draw()
        .unwrap();

    // Use a moving average to reduce noise; 7 runs seems to preserve the pattern
    // well enough while letting it be less spiky
    let mut ma = SumTreeSMA::<_, i64, 7>::new();

    ctx.draw_series(LineSeries::new(
        acc.iter().map(|dp| {
            ma.add_sample(dp.duration_minutes);
            (dp.started_at, ma.get_average())
        }),
        &BLUE,
    ))
    .unwrap();

    Ok(())
}

fn check_outlier(previous_duration: i64, current_duration: i64, outlier_multiplier: i64) -> bool {
    // If the multiplier is 0, we don't want to filter anything
    // If the previous duration is 0, we don't have a baseline to compare against
    if previous_duration == 0 || outlier_multiplier == 0 {
        return true;
    }

    let lower_bound = previous_duration / outlier_multiplier;
    let upper_bound = previous_duration * outlier_multiplier;

    current_duration.clamp(lower_bound, upper_bound) == current_duration
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_gh_header_parse() {
        let input = "<https://example.com>; rel=\"prev\", <https://example.com>; rel=\"next\", <https://example.com>; rel=\"last\", <https://example.com>; rel=\"first\"";

        let input = reqwest::header::HeaderValue::from_str(input).unwrap();

        assert_eq!(GhLinkHeader::from_header(&input).unwrap().len(), 4);
    }

    #[test]
    fn test_gh_header_individual() {
        let input = "<https://example.com>; rel=\"prev\"";

        assert_eq!(
            GhLinkHeader::try_from(input).unwrap(),
            GhLinkHeader {
                link: "https://example.com".parse().unwrap(),
                rel: Rel::Previous
            }
        )
    }
}
