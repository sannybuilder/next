import { Component } from '@angular/core';
import { EditorService } from '../../state/editor.service';

@Component({
  selector: 'app-terminal',
  imports: [],
  templateUrl: './terminal.component.html',
  styleUrl: './terminal.component.scss',
})
export class TerminalComponent {
  constructor(public editor: EditorService) {}
}
