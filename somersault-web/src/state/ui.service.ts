import { Injectable, signal } from '@angular/core';
import { persistentSignal } from './persistent-signal';

@Injectable({
  providedIn: 'root',
})
export class UiService {
  title = signal('');
  currentPage = signal(0);
  modified = signal(0);
  id = signal('');

  sidebarCollapsed = persistentSignal('next.ui.sidebar', false);
}
