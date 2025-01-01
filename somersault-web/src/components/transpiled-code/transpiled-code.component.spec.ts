import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TranspiledCodeComponent } from './transpiled-code.component';

describe('TranspiledCodeComponent', () => {
  let component: TranspiledCodeComponent;
  let fixture: ComponentFixture<TranspiledCodeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [TranspiledCodeComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(TranspiledCodeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
