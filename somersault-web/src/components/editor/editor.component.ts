import {
  Component,
  computed,
  effect,
  ElementRef,
  signal,
  ViewChild,
} from '@angular/core';
import { EditorService } from '../../state/editor.service';

import {
  lineNumbers,
  highlightActiveLineGutter,
  highlightSpecialChars,
  drawSelection,
  dropCursor,
  rectangularSelection,
  crosshairCursor,
  highlightActiveLine,
  keymap,
  hoverTooltip,
} from '@codemirror/view';
import { EditorView } from '@codemirror/view';
import { Compartment, EditorState } from '@codemirror/state';
import {
  foldGutter,
  indentOnInput,
  bracketMatching,
  foldKeymap,
  LanguageSupport,
} from '@codemirror/language';
import {
  history,
  defaultKeymap,
  historyKeymap,
  indentWithTab,
} from '@codemirror/commands';
import { highlightSelectionMatches, searchKeymap } from '@codemirror/search';
import {
  closeBrackets,
  autocompletion,
  closeBracketsKeymap,
  completionKeymap,
  completeFromList,
  snippetCompletion,
} from '@codemirror/autocomplete';
import { lintKeymap } from '@codemirror/lint';

import { javascript, javascriptLanguage } from '@codemirror/lang-javascript';
import * as themes from '@uiw/codemirror-themes-all';
import { persistentSignal } from '../../state/persistent-signal';
import {
  braceify,
  stringify,
  inputParams,
  outputParams,
  Command,
  stringifyWithColonAndCurly,
} from './sbl';

@Component({
  selector: 'app-editor',
  imports: [],
  templateUrl: './editor.component.html',
  styleUrl: './editor.component.scss',
})
export class EditorComponent {
  @ViewChild('editor') codeArea!: ElementRef<HTMLTextAreaElement>;
  ed!: EditorView;

  cursorPosition = signal({ line: 1, ch: 1 });
  sscVersion = this._editor.sscVersion;

  definitions = computed<{
    meta: { version: string };
    extensions: Array<{ name: string; commands: Command[] }>;
  }>(() => {
    try {
      return JSON.parse(this._editor.definitions());
    } catch {
      return {
        meta: {
          version: '',
        },
        extensions: [],
      };
    }
  });
  sblVersion = computed(() => this.definitions().meta.version);

  themeConfig = new Compartment();
  currentTheme = persistentSignal('next.ui.theme', 'vscode');

  changeDebounce: any = null;

  constructor(private _editor: EditorService) {
    effect(() => {
      const [initialSource] = this._editor.initialSource();
      const definitions = this.definitions();
      if (this.ed) this.ed.setState(this.newState(initialSource));
    });
  }

  ngAfterViewInit() {
    const [initialSource] = this._editor.initialSource();
    this.ed = new EditorView({
      state: this.newState(initialSource),
      parent: this.codeArea.nativeElement,
    });
  }

  changeTheme(theme: string) {
    let extension = this.getThemeExtension(theme);

    if (!extension) {
      return;
    }

    this.currentTheme.set(theme);

    const { init, style } = extension;

    this.ed.dispatch({
      effects: this.themeConfig.reconfigure(
        init({
          settings: {
            gutterBackground: style.background,
            gutterForeground: style.foreground,
            fontFamily: 'monospace',
          },
        })
      ),
    });
  }

  onThemeChange(event: any) {
    const theme = event.target.value.toLowerCase() as string;
    this.changeTheme(theme);
  }

  getThemeExtension(theme: string) {
    switch (theme) {
      case 'abcdef': {
        return { init: themes.abcdefInit, style: themes.defaultSettingsAbcdef };
      }
      case 'abyss': {
        return { init: themes.abyssInit, style: themes.defaultSettingsAbyss };
      }
      case 'androidstudio': {
        return {
          init: themes.androidstudioInit,
          style: themes.defaultSettingsAndroidstudio,
        };
      }
      case 'andromeda': {
        return {
          init: themes.andromedaInit,
          style: themes.defaultSettingsAndromeda,
        };
      }
      case 'atomone': {
        return {
          init: themes.atomoneInit,
          style: themes.defaultSettingsAtomone,
        };
      }
      case 'aura': {
        return { init: themes.auraInit, style: themes.defaultSettingsAura };
      }
      case 'basic': {
        return {
          init: themes.basicDarkInit,
          style: themes.defaultSettingsBasicDark,
        };
      }
      case 'bbedit': {
        return { init: themes.bbeditInit, style: themes.defaultSettingsBbedit };
      }
      case 'bespin': {
        return { init: themes.bespinInit, style: themes.defaultSettingsBespin };
      }
      case 'console': {
        return {
          init: themes.consoleDarkInit,
          style: themes.defaultSettingsConsoleDark,
        };
      }
      case 'copilot': {
        return {
          init: themes.copilotInit,
          style: themes.defaultSettingsCopilot,
        };
      }
      case 'darcula': {
        return {
          init: themes.darculaInit,
          style: themes.defaultSettingsDarcula,
        };
      }
      case 'dracula': {
        return {
          init: themes.draculaInit,
          style: themes.defaultSettingsDracula,
        };
      }
      case 'duotone': {
        return {
          init: themes.duotoneDarkInit,
          style: themes.defaultSettingsDuotoneDark,
        };
      }
      case 'eclipse': {
        return {
          init: themes.eclipseInit,
          style: themes.defaultSettingsEclipse,
        };
      }
      case 'github': {
        return {
          init: themes.githubDarkInit,
          style: themes.defaultSettingsGithubDark,
        };
      }
      case 'kimbie': {
        return { init: themes.kimbieInit, style: themes.defaultSettingsKimbie };
      }
      case 'material': {
        return {
          init: themes.materialInit,
          style: themes.defaultSettingsMaterial,
        };
      }
      case 'monokai': {
        return {
          init: themes.monokaiInit,
          style: themes.defaultSettingsMonokai,
        };
      }
      case 'nord': {
        return { init: themes.nordInit, style: themes.defaultSettingsNord };
      }
      case 'okaidia': {
        return {
          init: themes.okaidiaInit,
          style: themes.defaultSettingsOkaidia,
        };
      }
      case 'quietlight': {
        return {
          init: themes.quietlightInit,
          style: themes.defaultSettingsQuietlight,
        };
      }
      case 'red': {
        return { init: themes.redInit, style: themes.defaultSettingsRed };
      }
      case 'solarized': {
        return {
          init: themes.solarizedDarkInit,
          style: themes.defaultSettingsSolarizedDark,
        };
      }
      case 'sublime': {
        return {
          init: themes.sublimeInit,
          style: themes.defaultSettingsSublime,
        };
      }
      case 'vscode': {
        return {
          init: themes.vscodeDarkInit,
          style: themes.defaultSettingsVscodeDark,
        };
      }
      case 'xcode': {
        return {
          init: themes.xcodeDarkInit,
          style: themes.defaultSettingsXcodeDark,
        };
      }

      default:
        return;
    }
  }

  newState(content: string) {
    let { init, style } = this.getThemeExtension(this.currentTheme())!;

    const keywords = [
      'int',
      'float',
      'string',
      'return',
      'break',
      'continue',
      'const',
    ];

    const definitions = this.definitions();
    const snippets = definitions.extensions.flatMap((e) => {
      return e.commands
        .filter((c) => {
          return !c.attrs?.is_unsupported;
        })
        .flatMap((c) => {
          const input = c.input?.length
            ? braceify(
                stringify(inputParams(c), ', ', stringifyWithColonAndCurly),
                '()'
              )
            : [];

          return snippetCompletion([c.name.toLowerCase(), input].join(''), {
            label: c.name.toLowerCase(),
            type: 'function',
            detail: createFunctionDef(c),
            info: c.short_desc,
          });
        });
    });
    const exampleCompletion = javascriptLanguage.data.of({
      autocomplete: completeFromList([
        ...keywords.map((label) => ({ label, type: 'keyword' })),

        snippetCompletion('function ${name}(${params})\n\t${}\nend', {
          label: 'function',
          type: 'keyword',
        }),
        snippetCompletion('if ${} then\n\t${}\nend', {
          label: 'if',
          type: 'keyword',
        }),

        snippetCompletion('while ${}\n\t${}\nend', {
          label: 'while',
          type: 'keyword',
          detail: 'loop',
        }),
        ...snippets,
      ]),
    });
    const basicSetup = (() => [
      EditorView.updateListener.of((v) => {
        if (v.docChanged) {
          if (this.changeDebounce) {
            clearTimeout(this.changeDebounce);
            this.changeDebounce = null;
          }

          this.changeDebounce = setTimeout(() => {
            const text = this.ed.state.doc.toString();
            this._editor.source.set(text);
          }, 1000);
        }
        if (v.selectionSet) {
          const { head } = this.ed.state.selection.main;

          const wordAtCursor = this.ed.state.doc.sliceString(
            this.ed.state.selection.main.from,
            this.ed.state.selection.main.to
          );

          const cursor = this.ed.state.doc.lineAt(head);
          this.cursorPosition.set({
            ch: head - cursor.from,
            line: cursor.number,
          });
        }
      }),
      lineNumbers(),
      EditorView.lineWrapping,
      highlightActiveLineGutter(),
      highlightSpecialChars(),
      history(),
      foldGutter(),
      drawSelection(),
      dropCursor(),
      EditorState.allowMultipleSelections.of(true),
      indentOnInput(),
      // syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
      bracketMatching(),
      closeBrackets(),
      autocompletion(),
      rectangularSelection(),
      crosshairCursor(),
      highlightActiveLine(),
      highlightSelectionMatches(),
      keymap.of([
        ...closeBracketsKeymap,
        ...defaultKeymap,
        ...searchKeymap,
        ...historyKeymap,
        ...foldKeymap,
        ...completionKeymap,
        ...lintKeymap,
        indentWithTab,
      ]),
      new LanguageSupport(javascriptLanguage, [exampleCompletion]),
      this.themeConfig.of(
        init({
          settings: {
            gutterBackground: style.background,
            gutterForeground: style.foreground,
            fontFamily: 'monospace',
          },
        })
      ),

      hoverTooltip((view, pos, side) => {
        let { from, to, text } = view.state.doc.lineAt(pos);
        let start = pos,
          end = pos;
        while (start > from && /\w/.test(text[start - from - 1])) start--;
        while (end < to && /\w/.test(text[end - from])) end++;
        if ((start == pos && side < 0) || (end == pos && side > 0)) return null;

        const word = text.slice(start - from, end - from);

        const extensions = this.definitions().extensions;

        for (const e of extensions) {
          for (const c of e.commands) {
            if (c.name.toLowerCase() === word) {
              const desc = c.short_desc || '* no description yet *';
              return {
                pos: start,
                end,
                above: true,
                create(view) {
                  let dom = document.createElement('div');
                  dom.classList.add('tooltip');
                  dom.innerHTML =
                    desc +
                    '<br><br>' +
                    c.name.toLowerCase() +
                    createFunctionDef(c);
                  return { dom };
                },
              };
            }
          }
        }

        return null;
      }),
    ])();

    return EditorState.create({
      doc: content,
      extensions: [basicSetup],
    });
  }
}

function createFunctionDef(command: Command) {
  let params = '()';
  if (command.input?.length) {
    params = braceify(stringify(inputParams(command), ', '), '()');
  }

  if (command.output?.length) {
    params = params + ': ';
    if (command.attrs?.is_condition) {
      params += 'optional ';
    }
    return (
      params +
      outputParams(command)
        .map((p) => p.type)
        .join(', ')
    );
  }

  if (command.attrs?.is_condition) {
    return `${params}: logical`;
  }

  return params;
}
