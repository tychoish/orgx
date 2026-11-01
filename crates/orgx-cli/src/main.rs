mod emacs;
mod repl;

use clap::{Parser, Subcommand};
use emacs::EmacsClient;

#[derive(Parser, Debug)]
#[command(name = "orgx", about = "CLI and REPL interface for Emacs Org capture")]
struct Cli {
    #[arg(short, long, help = "Explicit Emacs server socket or host:port:key")]
    target: Option<String>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(about = "Single-shot capture into Org target")]
    Capture {
        #[arg(short, long, help = "Target Org file")]
        file: Option<String>,
        #[arg(short = 'H', long, help = "Target headline")]
        headline: Option<String>,
        #[arg(
            short,
            long,
            default_value = "task",
            help = "Template (task, note, journal, plain)"
        )]
        template: String,
        #[arg(help = "Entry text to capture")]
        text: Vec<String>,
    },
    #[command(about = "Start interactive terminal REPL")]
    Repl {
        #[arg(short, long, help = "Initial target Org file")]
        file: Option<String>,
        #[arg(short = 'H', long, help = "Initial target headline")]
        headline: Option<String>,
        #[arg(short, long, help = "Initial template")]
        template: Option<String>,
    },
    #[command(about = "Display Emacs connection and target status")]
    Status,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let client = EmacsClient::new(cli.target.as_deref())?;

    match cli.command {
        Some(Commands::Status) => {
            println!("Target: {}", client.target);
            match client.ping() {
                Ok(msg) => println!("Status: OK ({})", msg),
                Err(e) => println!("Status: Error ({})", e),
            }
            let (def_file, def_hl) = client.resolve_default_target(".")?;
            println!("Default capture target: {} [{}]", def_file, def_hl);
        }
        Some(Commands::Capture {
            file,
            headline,
            template,
            text,
        }) => {
            let (def_file, def_hl) = client.resolve_default_target(".")?;
            let target_file = file.unwrap_or(def_file);
            let target_hl = headline.unwrap_or(def_hl);
            let entry_text = if text.is_empty() {
                // Read all available input from stdin (supports pipes and multiline notes)
                use std::io::Read;
                let mut input = String::new();
                std::io::stdin().read_to_string(&mut input)?;
                input.trim().to_string()
            } else {
                text.join(" ")
            };

            if entry_text.is_empty() {
                eprintln!("Error: No text provided for capture.");
                std::process::exit(1);
            }

            let res = client.capture(&target_file, &target_hl, &template, &entry_text)?;
            println!("{}", res);
        }
        Some(Commands::Repl {
            file,
            headline,
            template,
        }) => {
            repl::run_repl(&client, file, headline, template)?;
        }
        None => {
            // Default to REPL
            repl::run_repl(&client, None, None, None)?;
        }
    }
    Ok(())
}
