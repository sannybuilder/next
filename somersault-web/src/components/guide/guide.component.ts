import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { RouterModule } from '@angular/router';
import { EditorComponent } from '../editor/editor.component';
import pages from './content';
import { UiService } from '../../state/ui.service';

@Component({
  selector: 'app-guide',
  imports: [RouterModule, CommonModule, EditorComponent],
  templateUrl: './guide.component.html',
  styleUrl: './guide.component.scss',
})
export class GuideComponent {
  pages = pages;
  currentPage = this._ui.currentPage;
  title = this._ui.title;

  sidebarCollapsed = this._ui.sidebarCollapsed;

  constructor(private _ui: UiService) {}

  isValidPage(pageId: number): boolean {
    return pageId >= 0 && pageId < pages.length;
  }
}
