use std::borrow::Cow;
use std::path::PathBuf;
use reedline::{
    DefaultCompleter, FileBackedHistory, Prompt, PromptEditMode, PromptHistorySearch,
    Reedline, Signal,
};

use crate::emacs::EmacsClient;

pub struct OrgxPrompt {
    pub target_file: String,
    pub headline: String,
    pub template: String,
}

impl Prompt for OrgxPrompt {
    fn render_prompt_left(&self) -> Cow<'_, str> {
        let f = std::path::Path::new(&self.target_file)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(&self.target_file);
        let hl = if self.headline.is_empty() {
            "".to_string()
        } else {
            format!(":{}", self.headline)
        };
        Cow::Owned(format!("orgx [{}{}] ({})> ", f, hl, self.template))
    }

    fn render_prompt_right(&self) -> Cow<'_, str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _edit_mode: PromptEditMode) -> Cow<'_, str> {
        Cow::Borrowed("")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        Cow::Borrowed("::: ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: PromptHistorySearch,
    ) -> Cow<'_, str> {
        Cow::Borrowed("(search) ")
    }
}

pub fn run_repl(
    client: &EmacsClient,
    init_file: Option<String>,
    init_headline: Option<String>,
    init_template: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let (def_file, def_hl) = client.resolve_default_target(".")?;
    let mut current_file = init_file.unwrap_or(def_file);
    let mut current_headline = init_headline.unwrap_or(def_hl);
    let mut current_template = init_template.unwrap_or_else(|| "task".to_string());

    // Setup history with XDG_CACHE_HOME support and in-memory fallback
    let cache_dir = if let Ok(xdg_cache) = std::env::var("XDG_CACHE_HOME") {
        PathBuf::from(xdg_cache).join("orgx")
    } else {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
        PathBuf::from(home).join(".cache").join("orgx")
    };
    let _ = std::fs::create_dir_all(&cache_dir);
    let history_file = cache_dir.join("history");
    let history: Box<dyn reedline::History> = match FileBackedHistory::with_file(1000, history_file) {
        Ok(f) => Box::new(f),
        Err(_) => Box::new(reedline::FileBackedHistory::default()),
    };

    // Setup completions
    let commands = vec![
        "/target".into(),
        "/template".into(),
        "/undo".into(),
        "/help".into(),
        "/clear".into(),
        "/quit".into(),
        "/exit".into(),
    ];
    let completer = Box::new(DefaultCompleter::new_with_wordlen(commands, 1));

    let mut line_editor = Reedline::create()
        .with_history(history)
        .with_completer(completer);

    println!("Orgx Capture Shell (Rust REPL)");
    println!("Type /help for commands, Ctrl-D or /quit to exit.\n");

    loop {
        let prompt = OrgxPrompt {
            target_file: current_file.clone(),
            headline: current_headline.clone(),
            template: current_template.clone(),
        };

        match line_editor.read_line(&prompt) {
            Ok(Signal::Success(line)) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }

                if trimmed.starts_with('/') {
                    let parts: Vec<&str> = trimmed[1..].split_whitespace().collect();
                    if parts.is_empty() {
                        continue;
                    }
                    match parts[0] {
                        "quit" | "exit" | "q" => break,
                        "clear" => {
                            let _ = line_editor.clear_scrollback();
                        }
                        "help" | "h" | "?" => {
                            println!("Available commands:");
                            println!("  /target [file[:headline]]  - Switch capture target");
                            println!("  /template [name]           - Switch template (task, note, journal, plain)");
                            println!("  /undo                      - Undo last capture in Emacs");
                            println!("  /clear                     - Clear scrollback");
                            println!("  /quit, /exit               - Exit REPL");
                        }
                        "target" | "t" => {
                            if parts.len() < 2 {
                                println!("Current target: {} [{}]", current_file, current_headline);
                            } else {
                                let arg = parts[1];
                                let split: Vec<&str> = arg.splitn(2, ':').collect();
                                current_file = split[0].to_string();
                                if split.len() > 1 {
                                    current_headline = split[1].to_string();
                                }
                                println!("Target set to {} [{}]", current_file, current_headline);
                            }
                        }
                        "template" | "tpl" => {
                            if parts.len() < 2 {
                                println!("Current template: {}", current_template);
                            } else {
                                current_template = parts[1].to_string();
                                println!("Template switched to: {}", current_template);
                            }
                        }
                        "undo" => {
                            match client.undo() {
                                Ok(msg) => println!("{}", msg),
                                Err(e) => eprintln!("Error undoing: {}", e),
                            }
                        }
                        other => {
                            println!("Unknown command: /{} (type /help for available commands)", other);
                        }
                    }
                } else {
                    match client.capture(&current_file, &current_headline, &current_template, trimmed) {
                        Ok(msg) => println!("{}", msg),
                        Err(e) => eprintln!("Capture error: {}", e),
                    }
                }
            }
            Ok(Signal::CtrlC) => {
                println!("^C");
            }
            Ok(Signal::CtrlD) => {
                println!();
                break;
            }
            Err(e) => {
                eprintln!("Error reading line: {}", e);
                break;
            }
        }
    }

    Ok(())
}
