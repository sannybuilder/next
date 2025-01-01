
export interface Attr {
    is_branch: boolean;
    is_segment: boolean;
    is_condition: boolean;
    is_nop: boolean;
    is_unsupported: boolean;
    is_constructor: boolean;
    is_destructor: boolean;
    is_static: boolean;
    is_overload: boolean;
    is_positional: boolean;
  }
  
  export type Attribute = keyof Attr;

  
export enum SourceType {
    any = 'any',
    var_any = 'var_any',
    var_global = 'var_global',
    var_local = 'var_local',
    literal = 'literal',
    pointer = 'pointer',
  }
  
  export interface Param {
    type: string;
    name: string;
    source?: SourceType;
  }
  
  export interface Command {
    id?: string;
    name: string;
    attrs?: Partial<Attr>;
    num_params: number;
    input?: Param[];
    output?: Param[];
    class?: string;
    member?: string;
    short_desc?: string;
    operator?: string;
    cc?: 'cdecl' | 'stdcall' | 'thiscall';
  }
  
  export function stringifySource(source?: SourceType) {
    switch (source) {
      case SourceType.var_any:
        return 'var';
      case SourceType.var_global:
        return 'global var';
      case SourceType.var_local:
        return 'local var';
      case SourceType.literal:
        return 'literal';
      case SourceType.pointer:
        return 'pointer';
      default:
        return '';
    }
  }
  
  export function stringifyWithColon(p: Param) {
    return [
      [
        stringifySource(p.source),
        p.name,
      ]
        .filter(Boolean)
        .join(' '),
      p.type,
    ]
      .filter(Boolean)
      .join(': ');
  }

  export function stringifyWithColonAndCurly(p: Param) {
    let input = [
      [
        stringifySource(p.source),
        p.name,
      ]
        .filter(Boolean)
        .join(' '),
      p.type,
    ]
      .filter(Boolean)
      .join(': ');

    return '${' + input + '}';
  }
  
  export function stringify(
    params: Param[],
    sep: ' ' | ', ',
    mapFn: (p: Param) => string = stringifyWithColon
  ): string {
    return params.map(mapFn).join(sep);
  }
  
  export function inputParams(command: Command) {
    return command?.input ?? [];
  }
  
  export function outputParams(command: Command) {
    return command?.output ?? [];
  }
  
  export function braceify(value: string, braces: '[]' | '()' | '{}') {
    return `${braces[0]}${value}${braces[1]}`;
  }